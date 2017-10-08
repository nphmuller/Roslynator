// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Roslynator.CSharp.Refactorings.ReplaceStatementWithIf;

namespace Roslynator.CSharp.Refactorings
{
    internal static class YieldStatementRefactoring
    {
        public static async Task ComputeRefactoringsAsync(RefactoringContext context, YieldStatementSyntax yieldStatement)
        {
            if (context.IsAnyRefactoringEnabled(
                    RefactoringIdentifiers.AddCastExpression,
                    RefactoringIdentifiers.CallToMethod)
                && yieldStatement.IsYieldReturn()
                && yieldStatement.Expression != null)
            {
                SemanticModel semanticModel = await context.GetSemanticModelAsync().ConfigureAwait(false);

                ISymbol memberSymbol = ReturnExpressionRefactoring.GetContainingMethodOrPropertySymbol(yieldStatement.Expression, semanticModel, context.CancellationToken);

                if (memberSymbol != null)
                {
                    SyntaxNode node = await memberSymbol
                        .DeclaringSyntaxReferences[0]
                        .GetSyntaxAsync(context.CancellationToken)
                        .ConfigureAwait(false);

                    TypeSyntax type = ReturnExpressionRefactoring.GetTypeOrReturnType(node);

                    if (type != null)
                    {
                        ITypeSymbol memberTypeSymbol = semanticModel.GetTypeSymbol(type, context.CancellationToken);

                        if (memberTypeSymbol?.SpecialType != SpecialType.System_Collections_IEnumerable)
                        {
                            ITypeSymbol typeSymbol = semanticModel.GetTypeSymbol(yieldStatement.Expression, context.CancellationToken);

                            if (context.IsAnyRefactoringEnabled(RefactoringIdentifiers.AddCastExpression, RefactoringIdentifiers.CallToMethod)
                                && yieldStatement.Expression.Span.Contains(context.Span)
                                && memberTypeSymbol?.IsNamedType() == true)
                            {
                                var namedTypeSymbol = (INamedTypeSymbol)memberTypeSymbol;

                                if (namedTypeSymbol.IsConstructedFromIEnumerableOfT())
                                {
                                    ITypeSymbol argumentSymbol = namedTypeSymbol.TypeArguments[0];

                                    if (argumentSymbol != typeSymbol)
                                    {
                                        ModifyExpressionRefactoring.ComputeRefactoring(
                                           context,
                                           yieldStatement.Expression,
                                           argumentSymbol,
                                           semanticModel);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            if (context.IsRefactoringEnabled(RefactoringIdentifiers.ReplaceStatementWithIfElse)
                && (context.Span.IsEmptyAndContainedInSpan(yieldStatement.YieldKeyword)
                    || context.Span.IsEmptyAndContainedInSpan(yieldStatement.ReturnOrBreakKeyword)
                    || context.Span.IsBetweenSpans(yieldStatement)))
            {
                await ReplaceStatementWithIfStatementRefactoring.ReplaceYieldReturnWithIfElse.ComputeRefactoringAsync(context, yieldStatement).ConfigureAwait(false);
            }
        }
    }
}
