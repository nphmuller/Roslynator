﻿// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;

#pragma warning disable CS0219, RCS1016, RCS1021, RCS1048, RCS1118, RCS1163, RCS1175, RCS1176, RCS1196

namespace Roslynator.CSharp.Analyzers.Tests
{
    internal static class UseMethodGroupInsteadOfAnonymousFunction
    {
        private class Foo
        {
            private void Bar()
            {
                IEnumerable<object> x = null;
                Func<string, object> project = null;
                Func<string, int, object> projectWithIndex = null;

                var items = new List<string>();

                x = items.Select(f => Project(f));

                x = items.Select(f =>
                {
                    return Project(f);
                });

                x = items.Select(delegate (string f)
                {
                    return Project(f);
                });

                x = items.Select((f, i) => ProjectWithIndex(f, i));

                x = items.Select((f, i) =>
                {
                    return ProjectWithIndex(f, i);
                });

                x = items.Select(delegate (string f, int i)
                {
                    return ProjectWithIndex(f, i);
                });

                project = f => Project(f);

                project = f =>
                {
                    return Project(f);
                };

                project = delegate (string f)
                {
                    return Project(f);
                };

                projectWithIndex = (f, i) => ProjectWithIndex(f, i);

                projectWithIndex = (f, i) =>
                {
                    return ProjectWithIndex(f, i);
                };

                projectWithIndex = delegate (string f, int i)
                {
                    return ProjectWithIndex(f, i);
                };

                items.ForEach(f => Do(f));

                items.ForEach(f =>
                {
                    Do(f);
                });

                items.ForEach((f) => Do(f));

                items.ForEach((f) =>
                {
                    Do(f);
                });

                items.ForEach(delegate (string f)
                {
                    Do(f);
                });

                x = items.Select(f => f.Project());

                x = items.Select(f =>
                {
                    return f.Project();
                });

                x = items.Select(delegate (string f)
                {
                    return f.Project();
                });

                x = items.Select((f, i) => f.ProjectWithIndex(i));

                x = items.Select((f, i) =>
                {
                    return f.ProjectWithIndex(i);
                });

                x = items.Select(delegate (string f, int i)
                {
                    return f.ProjectWithIndex(i);
                });

                project = f => f.Project();

                project = f =>
                {
                    return f.Project();
                };

                project = delegate (string f)
                {
                    return f.Project();
                };

                projectWithIndex = (f, i) => f.ProjectWithIndex(i);

                projectWithIndex = (f, i) =>
                {
                    return f.ProjectWithIndex(i);
                };

                projectWithIndex = delegate (string f, int i)
                {
                    return f.ProjectWithIndex(i);
                };

                items.ForEach(f => f.Do());

                items.ForEach(f =>
                {
                    f.Do();
                });

                items.ForEach((f) => f.Do());

                items.ForEach((f) =>
                {
                    f.Do();
                });

                items.ForEach(delegate (string f)
                {
                    f.Do();
                });
            }

            //n

            private void FunctionToAction()
            {
                string s = "";

                Action action = () => s.GetHashCode();
            }

            private void FunctionToAction(Action action)
            {
                action();

                string s = "";

                FunctionToAction(() => s.GetHashCode());
            }
        }

        private static string Project(this string value)
        {
            return value;
        }

        private static string ProjectWithIndex(this string value, int index)
        {
            return value;
        }

        private static void Do(this string value)
        {
        }
    }
}
