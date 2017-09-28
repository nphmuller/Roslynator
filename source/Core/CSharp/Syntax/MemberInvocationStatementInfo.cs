﻿// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Roslynator.CSharp.Syntax
{
    public struct MemberInvocationStatementInfo
    {
        private static MemberInvocationStatementInfo Default { get; } = new MemberInvocationStatementInfo();

        private MemberInvocationStatementInfo(
            InvocationExpressionSyntax invocationExpression,
            ExpressionSyntax expression,
            SimpleNameSyntax name,
            ArgumentListSyntax argumentList)
        {
            InvocationExpression = invocationExpression;
            Expression = expression;
            Name = name;
            ArgumentList = argumentList;
        }

        public InvocationExpressionSyntax InvocationExpression { get; }

        public ExpressionSyntax Expression { get; }

        public SimpleNameSyntax Name { get; }

        public ArgumentListSyntax ArgumentList { get; }

        public SeparatedSyntaxList<ArgumentSyntax> Arguments
        {
            get { return ArgumentList?.Arguments ?? default(SeparatedSyntaxList<ArgumentSyntax>); }
        }

        public ExpressionStatementSyntax ExpressionStatement
        {
            get { return (ExpressionStatementSyntax)InvocationExpression?.Parent; }
        }

        public MemberAccessExpressionSyntax MemberAccessExpression
        {
            get { return (MemberAccessExpressionSyntax)Expression?.Parent; }
        }

        public string NameText
        {
            get { return Name?.Identifier.ValueText; }
        }

        public bool Success
        {
            get { return InvocationExpression != null; }
        }

        internal static MemberInvocationStatementInfo Create(
            SyntaxNode node,
            SyntaxInfoOptions options = null)
        {
            return Create(node as ExpressionStatementSyntax, options);
        }

        internal static MemberInvocationStatementInfo Create(
            ExpressionStatementSyntax expressionStatement,
            SyntaxInfoOptions options = null)
        {
            return CreateCore(expressionStatement, options ?? SyntaxInfoOptions.Default);
        }

        internal static MemberInvocationStatementInfo CreateCore(
            ExpressionStatementSyntax expressionStatement,
            SyntaxInfoOptions options)
        {
            if (!(expressionStatement?.Expression is InvocationExpressionSyntax invocationExpression))
                return Default;

            if (!(invocationExpression.Expression is MemberAccessExpressionSyntax memberAccessExpression))
                return Default;

            if (memberAccessExpression.Kind() != SyntaxKind.SimpleMemberAccessExpression)
                return Default;

            ExpressionSyntax expression = memberAccessExpression.Expression;

            if (!options.Check(expression))
                return Default;

            SimpleNameSyntax name = memberAccessExpression.Name;

            if (!options.Check(name))
                return Default;

            ArgumentListSyntax argumentList = invocationExpression.ArgumentList;

            if (argumentList == null)
                return Default;

            return new MemberInvocationStatementInfo(
                invocationExpression,
                expression,
                name,
                argumentList);
        }

        public override string ToString()
        {
            return ExpressionStatement?.ToString() ?? base.ToString();
        }
    }
}
