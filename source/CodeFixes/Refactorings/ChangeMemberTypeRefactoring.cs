﻿// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Diagnostics;
using System.Linq;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslynator.CSharp.CodeFixes;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace Roslynator.CSharp.Refactorings
{
    internal static class ChangeMemberTypeRefactoring
    {
        public static void ComputeCodeFix(
             CodeFixContext context,
             Diagnostic diagnostic,
             ExpressionSyntax expression,
             SemanticModel semanticModel)
        {
            TypeInfo typeInfo = semanticModel.GetTypeInfo(expression, context.CancellationToken);

            ITypeSymbol expressionTypeSymbol = typeInfo.Type;

            if (expressionTypeSymbol == null)
                return;

            if (!expressionTypeSymbol.SupportsExplicitDeclaration())
                return;

            (ISymbol symbol, ITypeSymbol typeSymbol) = GetContainingSymbolAndType(expression, semanticModel, context.CancellationToken);

            Debug.Assert(symbol != null, expression.ToString());

            if (symbol == null)
                return;

            if (symbol.IsOverride)
                return;

            if (symbol.ImplementsInterfaceMember())
                return;

            SyntaxNode node = symbol.GetSyntax(context.CancellationToken);

            if (node.Kind() == SyntaxKind.VariableDeclarator)
                node = node.Parent.Parent;

            TypeSyntax type = GetTypeOrReturnType(node);

            Debug.Assert(type != null, node.ToString());

            if (type == null)
                return;

            ITypeSymbol newTypeSymbol = expressionTypeSymbol;

            bool insertAwait = false;

            if (symbol.IsAsyncMethod())
            {
                INamedTypeSymbol taskOfT = semanticModel.GetTypeByMetadataName(MetadataNames.System_Threading_Tasks_Task_T);

                if (taskOfT == null)
                    return;

                if (expression.Kind() == SyntaxKind.AwaitExpression)
                {
                    newTypeSymbol = taskOfT.Construct(expressionTypeSymbol);
                }
                else if (expressionTypeSymbol.IsConstructedFrom(taskOfT))
                {
                    insertAwait = true;
                }
                else if (expressionTypeSymbol.Equals(semanticModel.GetTypeByMetadataName(MetadataNames.System_Threading_Tasks_Task)))
                {
                    return;
                }
            }

            string additionalKey = null;

            if (newTypeSymbol is INamedTypeSymbol newNamedType)
            {
                INamedTypeSymbol orderedEnumerableSymbol = semanticModel.GetTypeByMetadataName(MetadataNames.System_Linq_IOrderedEnumerable_T);

                if (newNamedType.ConstructedFrom.Equals(orderedEnumerableSymbol))
                {
                    INamedTypeSymbol enumerableSymbol = semanticModel.GetTypeByMetadataName(MetadataNames.System_Collections_Generic_IEnumerable_T);

                    if (!typeSymbol.IsConstructedFrom(enumerableSymbol))
                    {
                        INamedTypeSymbol constructedEnumerableSymbol = enumerableSymbol.Construct(newNamedType.TypeArguments.ToArray());

                        RegisterCodeFix(context, diagnostic, node, type, expression, constructedEnumerableSymbol, semanticModel, insertAwait: insertAwait);
                        additionalKey = "IOrderedEnumerable<T>";
                    }
                }
            }

            RegisterCodeFix(context, diagnostic, node, type, expression, newTypeSymbol, semanticModel, insertAwait: insertAwait, additionalKey: additionalKey);
        }

        private static void RegisterCodeFix(
            CodeFixContext context,
            Diagnostic diagnostic,
            SyntaxNode node,
            TypeSyntax type,
            ExpressionSyntax expression,
            ITypeSymbol newTypeSymbol,
            SemanticModel semanticModel,
            bool insertAwait = false,
            string additionalKey = null)
        {
            Document document = context.Document;

            string typeName = SymbolDisplay.GetMinimalString(newTypeSymbol, semanticModel, type.SpanStart);

            string title = $"Change {GetText(node)} type to '{typeName}'";

            if (insertAwait)
                title += " and insert 'await'";

            CodeAction codeAction = CodeAction.Create(
                title,
                cancellationToken =>
                {
                    SyntaxNode newNode = null;

                    TypeSyntax newType = ParseTypeName(typeName)
                        .WithTriviaFrom(type);

                    if (insertAwait)
                    {
                        var nodes = new SyntaxNode[] { type, expression };

                        newNode = node.ReplaceNodes(nodes, (f, g) =>
                        {
                            if (f == type)
                            {
                                return newType;
                            }
                            else
                            {
                                return AwaitExpression(
                                    Token(expression.GetLeadingTrivia(), SyntaxKind.AwaitKeyword, TriviaList(Space)),
                                    expression.WithoutLeadingTrivia());
                            }
                        });

                        return document.ReplaceNodeAsync(node, newNode, cancellationToken);
                    }
                    else
                    {
                        return document.ReplaceNodeAsync(type, newType, cancellationToken);
                    }
                },
                EquivalenceKeyProvider.GetEquivalenceKey(diagnostic, CodeFixIdentifiers.ChangeMemberTypeAccordingToReturnExpression, additionalKey));

            context.RegisterCodeFix(codeAction, diagnostic);
        }

        private static (ISymbol symbol, ITypeSymbol typeSymbol) GetContainingSymbolAndType(
        ExpressionSyntax expression,
        SemanticModel semanticModel,
        CancellationToken cancellationToken = default(CancellationToken))
        {
            switch (semanticModel.GetEnclosingSymbol(expression.SpanStart, cancellationToken))
            {
                case IMethodSymbol methodSymbol:
                    {
                        MethodKind methodKind = methodSymbol.MethodKind;

                        if (methodKind == MethodKind.PropertyGet)
                        {
                            var propertySymbol = (IPropertySymbol)methodSymbol.AssociatedSymbol;

                            return (propertySymbol, propertySymbol.Type);
                        }

                        if (methodKind == MethodKind.Ordinary
                            && methodSymbol.PartialImplementationPart != null)
                        {
                            methodSymbol = methodSymbol.PartialImplementationPart;
                        }

                        return (methodSymbol, methodSymbol.ReturnType);
                    }
                case IFieldSymbol fieldSymbol:
                    {
                        return (fieldSymbol, fieldSymbol.Type);
                    }
            }

            Debug.Fail(expression.ToString());

            return default((ISymbol symbol, ITypeSymbol typeSymbol));
        }

        private static TypeSyntax GetTypeOrReturnType(SyntaxNode node)
        {
            switch (node.Kind())
            {
                case SyntaxKind.MethodDeclaration:
                    return ((MethodDeclarationSyntax)node).ReturnType;
                case SyntaxKind.PropertyDeclaration:
                    return ((PropertyDeclarationSyntax)node).Type;
                case SyntaxKind.IndexerDeclaration:
                    return ((IndexerDeclarationSyntax)node).Type;
                case SyntaxKind.LocalFunctionStatement:
                    return ((LocalFunctionStatementSyntax)node).ReturnType;
                case SyntaxKind.FieldDeclaration:
                    return ((FieldDeclarationSyntax)node).Declaration.Type;
                default:
                    return null;
            }
        }

        private static string GetText(SyntaxNode node)
        {
            switch (node.Kind())
            {
                case SyntaxKind.MethodDeclaration:
                case SyntaxKind.LocalFunctionStatement:
                    return "return";
                case SyntaxKind.PropertyDeclaration:
                    return "property";
                case SyntaxKind.IndexerDeclaration:
                    return "indexer";
                case SyntaxKind.FieldDeclaration:
                    return "field";
                default:
                    return null;
            }
        }
    }
}