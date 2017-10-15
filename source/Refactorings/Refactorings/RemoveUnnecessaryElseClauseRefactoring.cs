﻿// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using static Roslynator.CSharp.CSharpFactory;

namespace Roslynator.CSharp.Refactorings
{
    internal static class RemoveUnnecessaryElseClauseRefactoring
    {
        public static void ComputeRefactoring(RefactoringContext context, ElseClauseSyntax elseClause)
        {
            if (elseClause.Statement?.IsKind(SyntaxKind.IfStatement) != false)
                return;

            if (!(elseClause.Parent is IfStatementSyntax ifStatement))
                return;

            if (!ifStatement.IsTopmostIf())
                return;

            StatementSyntax statement = ifStatement.Statement;

            if (statement is BlockSyntax block)
                statement = block.Statements.LastOrDefault();

            if (statement?.Kind().IsJumpStatement() != true)
                return;

            context.RegisterRefactoring(
                "Remove else clause",
                cancellationToken => RefactorAsync(context.Document, ifStatement, elseClause, cancellationToken));
        }

        private static Task<Document> RefactorAsync(
            Document document,
            IfStatementSyntax ifStatement,
            ElseClauseSyntax elseClause,
            CancellationToken cancellationToken = default(CancellationToken))
        {
            List<StatementSyntax> newStatements = null;

            if (elseClause.Statement is BlockSyntax block)
            {
                SyntaxList<StatementSyntax> statements = block.Statements;

                if (!statements.Any())
                    return document.RemoveNodeAsync(elseClause, SyntaxRemoveOptions.KeepExteriorTrivia, cancellationToken);

                newStatements = new List<StatementSyntax>() { ifStatement.WithElse(null) };

                StatementSyntax lastStatement = statements.Last();

                SyntaxTriviaList leadingTrivia = lastStatement.GetLeadingTrivia();

                if (!leadingTrivia.Contains(SyntaxKind.EndOfLineTrivia))
                    statements = statements.Replace(lastStatement, lastStatement.WithLeadingTrivia(leadingTrivia.Insert(0, NewLine())));

                foreach (StatementSyntax statement in statements)
                    newStatements.Add(statement.WithFormatterAnnotation());
            }
            else
            {
                StatementSyntax statement = elseClause.Statement;

                SyntaxTriviaList leadingTrivia = statement.GetLeadingTrivia();

                if (!leadingTrivia.Contains(SyntaxKind.EndOfLineTrivia))
                    statement = statement.WithLeadingTrivia(leadingTrivia.Insert(0, NewLine()));

                statement = statement.WithFormatterAnnotation();

                newStatements = new List<StatementSyntax>() { ifStatement.WithElse(null), statement };
            }

            if (ifStatement.IsEmbedded())
            {
                BlockSyntax newBlock = Block(newStatements).WithFormatterAnnotation();

                return document.ReplaceNodeAsync(ifStatement, newBlock, cancellationToken);
            }
            else
            {
                return document.ReplaceNodeAsync(ifStatement, newStatements, cancellationToken);
            }
        }
    }
}