﻿// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslynator.CSharp.Refactorings;

namespace Roslynator.CSharp.CodeFixes
{
    internal static class CodeFixRegistrator
    {
        public static void AddCastExpression(
            CodeFixContext context,
            Diagnostic diagnostic,
            ExpressionSyntax expression,
            ITypeSymbol destinationType,
            SemanticModel semanticModel)
        {
            string typeName = SymbolDisplay.GetMinimalString(destinationType, semanticModel, expression.SpanStart);

            TypeSyntax newType = SyntaxFactory.ParseTypeName(typeName);

            CodeAction codeAction = CodeAction.Create(
                $"Cast to '{typeName}'",
                cancellationToken => AddCastExpressionRefactoring.RefactorAsync(context.Document, expression, newType, cancellationToken),
                EquivalenceKeyProvider.GetEquivalenceKey(diagnostic, CodeFixIdentifiers.AddCastExpression));

            context.RegisterCodeFix(codeAction, diagnostic);
        }

        public static void RemoveMember(CodeFixContext context, Diagnostic diagnostic, MemberDeclarationSyntax memberDeclaration, string additionalKey = null)
        {
            Document document = context.Document;

            CodeAction codeAction = CodeAction.Create(
                $"Remove {memberDeclaration.GetTitle()}",
                cancellationToken => document.RemoveMemberAsync(memberDeclaration, cancellationToken),
                EquivalenceKeyProvider.GetEquivalenceKey(diagnostic, additionalKey));

            context.RegisterCodeFix(codeAction, diagnostic);
        }
    }
}
