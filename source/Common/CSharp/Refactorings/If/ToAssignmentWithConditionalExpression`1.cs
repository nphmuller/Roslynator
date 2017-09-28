﻿// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslynator.CSharp.Syntax;

namespace Roslynator.CSharp.Refactorings.If
{
    internal abstract class ToAssignmentWithConditionalExpression<TStatement> : ToAssignmentWithConditionalExpression
        where TStatement : StatementSyntax
    {
        protected ToAssignmentWithConditionalExpression(
            TStatement statement,
            IfStatementSyntax ifStatement,
            ExpressionSyntax whenTrue,
            ExpressionSyntax whenFalse) : base(ifStatement, whenTrue, whenFalse)
        {
            Statement = statement;
        }

        public TStatement Statement { get; }

        protected abstract TStatement CreateNewStatement();

        public override Task<Document> RefactorAsync(
            Document document,
            CancellationToken cancellationToken = default(CancellationToken))
        {
            StatementsInfo statementsInfo = SyntaxInfo.StatementsInfo(IfStatement);

            SyntaxList<StatementSyntax> statements = statementsInfo.Statements;

            int index = statements.IndexOf(IfStatement);

            TStatement newStatement = CreateNewStatement();

                SyntaxList<StatementSyntax> newStatements = statements
                .RemoveAt(index)
                .ReplaceAt(index - 1, newStatement);

            return document.ReplaceStatementsAsync(statementsInfo, newStatements, cancellationToken);
        }
    }
}
