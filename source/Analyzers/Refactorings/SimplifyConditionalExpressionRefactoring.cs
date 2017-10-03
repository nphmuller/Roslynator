﻿// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Text;
using Roslynator.CSharp;
using Roslynator.CSharp.Syntax;

namespace Roslynator.CSharp.Refactorings
{
    internal static class SimplifyConditionalExpressionRefactoring
    {
        public static void Analyze(SyntaxNodeAnalysisContext context, ConditionalExpressionSyntax conditionalExpression)
        {
            if (context.Node.ContainsDiagnostics)
                return;

            if (context.Node.SpanContainsDirectives())
                return;

            ConditionalExpressionInfo info = SyntaxInfo.ConditionalExpressionInfo(conditionalExpression);

            if (!info.Success)
                return;

            switch (info.WhenTrue.Kind())
            {
                case SyntaxKind.TrueLiteralExpression:
                    {
                        if (info.WhenFalse.IsKind(SyntaxKind.FalseLiteralExpression))
                            context.ReportDiagnostic(DiagnosticDescriptors.SimplifyConditionalExpression, conditionalExpression);

                        break;
                    }
                case SyntaxKind.FalseLiteralExpression:
                    {
                        if (info.WhenFalse.IsKind(SyntaxKind.TrueLiteralExpression))
                            context.ReportDiagnostic(DiagnosticDescriptors.SimplifyConditionalExpression, conditionalExpression);

                        break;
                    }
            }
        }

        public static async Task<Document> RefactorAsync(
            Document document,
            ConditionalExpressionSyntax conditionalExpression,
            CancellationToken cancellationToken)
        {
            SemanticModel semanticModel = await document.GetSemanticModelAsync(cancellationToken).ConfigureAwait(false);

            ExpressionSyntax condition = conditionalExpression.Condition;

            ExpressionSyntax newNode = (conditionalExpression.WhenTrue.WalkDownParentheses().IsKind(SyntaxKind.TrueLiteralExpression))
                ? condition
                : CSharpUtility.LogicallyNegate(condition, semanticModel, cancellationToken);

            SyntaxTriviaList trailingTrivia = conditionalExpression
                .DescendantTrivia(TextSpan.FromBounds(condition.Span.End, conditionalExpression.Span.End))
                .ToSyntaxTriviaList()
                .EmptyIfWhitespace()
                .AddRange(conditionalExpression.GetTrailingTrivia());

            newNode = newNode
                .WithLeadingTrivia(conditionalExpression.GetLeadingTrivia())
                .WithTrailingTrivia(trailingTrivia);

            return await document.ReplaceNodeAsync(conditionalExpression, newNode, cancellationToken).ConfigureAwait(false);
        }
    }
}
