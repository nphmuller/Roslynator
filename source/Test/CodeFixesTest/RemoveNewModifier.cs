﻿// Copyright (c) Josef Pihrt. All rights reserved. Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace Roslynator.CSharp.CodeFixes.Test
{
    internal static class RemoveNewModifier
    {
        private class Foo
        {
            new public void Bar()
            {
            }
        }
    }
}