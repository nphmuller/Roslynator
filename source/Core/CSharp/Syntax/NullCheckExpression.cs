﻿// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Threading;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslynator.Utilities;

namespace Roslynator.CSharp.Syntax
{
    internal struct NullCheckExpression
    {
        public NullCheckExpression(
            ExpressionSyntax node,
            ExpressionSyntax expression,
            NullCheckKind kind)
        {
            Node = node;
            Expression = expression;
            Kind = kind;
        }

        public ExpressionSyntax Node { get; }

        public ExpressionSyntax Expression { get; }

        public NullCheckKind Kind { get; }

        public bool IsCheckingNull
        {
            get { return (Kind & NullCheckKind.IsNull) != 0; }
        }

        public bool IsCheckingNotNull
        {
            get { return (Kind & NullCheckKind.IsNotNull) != 0; }
        }

        public static bool TryCreate(
            SyntaxNode node,
            out NullCheckExpression result,
            bool walkDownParentheses = true,
            NullCheckKind allowedKinds = NullCheckKind.ComparisonToNull)
        {
            if ((allowedKinds & NullCheckKind.HasValueProperty) == 0)
            {
                return TryCreate(
                    node,
                    default(SemanticModel),
                    out result,
                    walkDownParentheses,
                    allowedKinds,
                    default(CancellationToken));
            }

            result = default(NullCheckExpression);
            return false;
        }

        public static bool TryCreate(
            SyntaxNode node,
            SemanticModel semanticModel,
            out NullCheckExpression result,
            bool walkDownParentheses = true,
            NullCheckKind allowedKinds = NullCheckKind.All,
            CancellationToken cancellationToken = default(CancellationToken))
        {
            ExpressionSyntax expression = (node as ExpressionSyntax)?.WalkDownParenthesesIf(walkDownParentheses);

            if (expression != null)
            {
                SyntaxKind kind = expression.Kind();

                if (kind == SyntaxKind.EqualsExpression
                    || kind == SyntaxKind.NotEqualsExpression)
                {
                    var binaryExpression = (BinaryExpressionSyntax)expression;

                    ExpressionSyntax left = binaryExpression.Left?.WalkDownParenthesesIf(walkDownParentheses);

                    if (left?.IsMissing == false)
                    {
                        ExpressionSyntax right = binaryExpression.Right?.WalkDownParenthesesIf(walkDownParentheses);

                        if (right?.IsMissing == false)
                        {
                            return TryCreate(binaryExpression, kind, left, right, semanticModel, cancellationToken, allowedKinds, out result)
                                || TryCreate(binaryExpression, kind, right, left, semanticModel, cancellationToken, allowedKinds, out result);
                        }
                    }
                }
                else if (kind == SyntaxKind.SimpleMemberAccessExpression)
                {
                    if ((allowedKinds & NullCheckKind.HasValue) != 0)
                    {
                        var memberAccessExpression = (MemberAccessExpressionSyntax)expression;

                        if (IsPropertyOfNullableOfT(memberAccessExpression.Name, "HasValue", semanticModel, cancellationToken))
                        {
                            result = new NullCheckExpression(expression, memberAccessExpression.Expression, NullCheckKind.HasValue);
                            return true;
                        }
                    }
                }
                else if (kind == SyntaxKind.LogicalNotExpression)
                {
                    if ((allowedKinds & NullCheckKind.NotHasValue) != 0)
                    {
                        var logicalNotExpression = (PrefixUnaryExpressionSyntax)expression;

                        ExpressionSyntax operand = logicalNotExpression.Operand?.WalkDownParenthesesIf(walkDownParentheses);

                        if (operand?.IsKind(SyntaxKind.SimpleMemberAccessExpression) == true)
                        {
                            var memberAccessExpression = (MemberAccessExpressionSyntax)operand;

                            if (IsPropertyOfNullableOfT(memberAccessExpression.Name, "HasValue", semanticModel, cancellationToken))
                            {
                                result = new NullCheckExpression(expression, memberAccessExpression.Expression, NullCheckKind.NotHasValue);
                                return true;
                            }
                        }
                    }
                }
            }

            result = default(NullCheckExpression);
            return false;
        }

        private static bool TryCreate(
            BinaryExpressionSyntax binaryExpression,
            SyntaxKind binaryExpressionKind,
            ExpressionSyntax expression1,
            ExpressionSyntax expression2,
            SemanticModel semanticModel,
            CancellationToken cancellationToken,
            NullCheckKind allowedKinds,
            out NullCheckExpression result)
        {
            SyntaxKind kind = expression1.Kind();

            if (kind == SyntaxKind.NullLiteralExpression)
            {
                return CreateIfAllowed(
                    binaryExpression,
                    expression2,
                    (binaryExpressionKind == SyntaxKind.EqualsExpression) ? NullCheckKind.EqualsToNull : NullCheckKind.NotEqualsToNull,
                    allowedKinds,
                    out result);
            }
            else if (kind == SyntaxKind.TrueLiteralExpression)
            {
                if ((allowedKinds & (NullCheckKind.HasValueProperty)) != 0
                    && expression2?.IsKind(SyntaxKind.SimpleMemberAccessExpression) == true)
                {
                    var memberAccessExpression = (MemberAccessExpressionSyntax)expression2;

                    if (IsPropertyOfNullableOfT(memberAccessExpression.Name, "HasValue", semanticModel, cancellationToken))
                    {
                        return CreateIfAllowed(
                            binaryExpression,
                            memberAccessExpression.Expression,
                            (binaryExpressionKind == SyntaxKind.EqualsExpression) ? NullCheckKind.HasValue : NullCheckKind.NotHasValue,
                            allowedKinds,
                            out result);
                    }
                }
            }
            else if (kind == SyntaxKind.FalseLiteralExpression)
            {
                if ((allowedKinds & NullCheckKind.HasValueProperty) != 0
                    && expression2?.IsKind(SyntaxKind.SimpleMemberAccessExpression) == true)
                {
                    var memberAccessExpression = (MemberAccessExpressionSyntax)expression2;

                    if (IsPropertyOfNullableOfT(memberAccessExpression.Name, "HasValue", semanticModel, cancellationToken))
                    {
                        return CreateIfAllowed(
                            binaryExpression,
                            memberAccessExpression.Expression,
                            (binaryExpressionKind == SyntaxKind.EqualsExpression) ? NullCheckKind.NotHasValue : NullCheckKind.HasValue,
                            allowedKinds,
                            out result);
                    }
                }
            }

            result = default(NullCheckExpression);
            return false;
        }

        private static bool CreateIfAllowed(
            ExpressionSyntax node,
            ExpressionSyntax expression,
            NullCheckKind kind,
            NullCheckKind allowedKinds,
            out NullCheckExpression result)
        {
            if ((allowedKinds & kind) != 0)
            {
                result = new NullCheckExpression(node, expression, kind);
                return true;
            }

            result = default(NullCheckExpression);
            return false;
        }

        private static bool IsPropertyOfNullableOfT(
            ExpressionSyntax expression,
            string name,
            SemanticModel semanticModel,
            CancellationToken cancellationToken = default(CancellationToken))
        {
            return expression?.IsKind(SyntaxKind.IdentifierName) == true
                && string.Equals(((IdentifierNameSyntax)expression).Identifier.ValueText, name, StringComparison.Ordinal)
                && SemanticUtilities.IsPropertyOfNullableOfT(expression, name, semanticModel, cancellationToken);
        }

        public override string ToString()
        {
            return Node?.ToString() ?? base.ToString();
        }
    }
}
