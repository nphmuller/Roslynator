// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Roslynator.CSharp.Refactorings
{
    internal static class ReturnExpressionRefactoring
    {
        public static async Task ComputeRefactoringsAsync(RefactoringContext context, ExpressionSyntax expression)
        {
            if (expression == null)
                return;

            SemanticModel semanticModel = await context.GetSemanticModelAsync().ConfigureAwait(false);

            ISymbol memberSymbol = GetContainingMethodOrPropertySymbol(expression, semanticModel, context.CancellationToken);

            if (memberSymbol == null)
                return;

            SyntaxNode node = await memberSymbol
                .DeclaringSyntaxReferences[0]
                .GetSyntaxAsync(context.CancellationToken)
                .ConfigureAwait(false);

            TypeSyntax type = GetTypeOrReturnType(node);

            if (type == null)
                return;

            ITypeSymbol memberTypeSymbol = semanticModel.GetTypeSymbol(type, context.CancellationToken);

            if (memberTypeSymbol?.IsErrorType() != false)
                return;

            ITypeSymbol expressionSymbol = semanticModel.GetTypeSymbol(expression, context.CancellationToken);

            if (expressionSymbol?.IsErrorType() != false)
                return;

            if (!context.IsAnyRefactoringEnabled(
                RefactoringIdentifiers.AddCastExpression,
                RefactoringIdentifiers.CallToMethod))
            {
                return;
            }

            ITypeSymbol castTypeSymbol = GetCastTypeSymbol(memberSymbol, memberTypeSymbol, expressionSymbol, semanticModel);

            if (castTypeSymbol == null)
                return;

            ModifyExpressionRefactoring.ComputeRefactoring(context, expression, castTypeSymbol, semanticModel);
        }

        private static ITypeSymbol GetCastTypeSymbol(
            ISymbol memberSymbol,
            ITypeSymbol memberTypeSymbol,
            ITypeSymbol expressionSymbol,
            SemanticModel semanticModel)
        {
            if (memberSymbol.IsAsyncMethod())
            {
                if (memberTypeSymbol.IsNamedType())
                {
                    var namedTypeSymbol = (INamedTypeSymbol)memberTypeSymbol;

                    INamedTypeSymbol taskOfTSymbol = semanticModel.GetTypeByMetadataName(MetadataNames.System_Threading_Tasks_Task_T);

                    if (taskOfTSymbol != null
                        && namedTypeSymbol.ConstructedFrom.Equals(taskOfTSymbol)
                        && !namedTypeSymbol.TypeArguments[0].Equals(expressionSymbol))
                    {
                        return namedTypeSymbol.TypeArguments[0];
                    }
                }
            }
            else if (!memberTypeSymbol.Equals(expressionSymbol))
            {
                return memberTypeSymbol;
            }

            return null;
        }

        internal static TypeSyntax GetTypeOrReturnType(SyntaxNode node)
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
                default:
                    return null;
            }
        }

        internal static string GetText(SyntaxNode node)
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
                default:
                    return null;
            }
        }

        internal static ISymbol GetContainingMethodOrPropertySymbol(
            ExpressionSyntax expression,
            SemanticModel semanticModel,
            CancellationToken cancellationToken = default(CancellationToken))
        {
            ISymbol symbol = semanticModel.GetEnclosingSymbol(expression.SpanStart, cancellationToken);

            if (symbol?.IsMethod() == true)
            {
                var methodsymbol = (IMethodSymbol)symbol;
                MethodKind methodKind = methodsymbol.MethodKind;

                if (methodKind == MethodKind.Ordinary)
                {
                    if (methodsymbol.PartialImplementationPart != null)
                        symbol = methodsymbol.PartialImplementationPart;
                }
                else if (methodKind == MethodKind.PropertyGet)
                {
                    symbol = methodsymbol.AssociatedSymbol;
                }
            }

            return symbol;
        }
    }
}
