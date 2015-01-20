{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{-| Use @turtle@ if you want to write light-weight and maintainable shell
    scripts.

    @turtle@ embeds shell scripting directly within Haskell for three key
    reasons:

    * Haskell is syntactically lightweight, thanks to global type inference
      which lets you omit all types

    * Haskell is not object-oriented, which means that top-level values and
      functions are cheap and idiomatic

    * Haskell code is easy to refactor and maintain because Haskell is
      statically typed

    These three features combine to give you the convenience of a scripting
    language with the safety and maintainability of static typing.

    This tutorial introduces how to use the @turtle@ library for scripting and
    the tutorial assumes no prior knowledge of Haskell, but does assume prior
    knowledge of Bash or a similar shell scripting language.
-}

module Turtle.Tutorial (
    -- * Introduction
    -- $introduction

    -- * Comparison - Part 1
    -- $compare1

    -- * @do@ notation
    -- $do
    ) where

import Turtle

-- $introduction
--  Let's translate some simple Bash scripts to Haskell and work our way up to
--  more complex scripts:
--
--  This is how you write a \"Hello, world\" script in both languages:
--
-- >#!/usr/bin/env runhaskell           -- #!/bin/bash
-- >                                    --
-- >{-# LANGUAGE OverloadedStrings #-}  --
-- >                                    --
-- >import Turtle                       --
-- >                                    --
-- >main = echo "Hello, world!"         -- echo Hello, world!
--
--  You can execute the above code by saving it to the file @example.hs@, making
--  the file executable, and then running the file:
--
-- > $ chmod u+x example.hs 
-- > $ ./example.hs
-- > Hello, world!
--
--  If you delete the first line of the program, you can also compile the above
--  code to generate a native executable:
--
-- > $ ghc -O2 example.hs
-- > $ ./example
-- > Hello, world!

-- $compare1
--  You'll already notice a few differences.
--
--  First, the Haskell code requires two additional lines of overhead to import
--  the @turtle@ library and enable overloading of string literals.  This
--  overhead is unavoidable.
--
--  Second, the Haskell `echo` explicitly quotes its `Text` argument whereas the
--  Bash @echo@ does not because Bash commonly blurs the lines between strings
--  and everything else.  Haskell actually does let you blur the line, too,
--  using the @OverloadedStrings@ extension.  We'll see examples of this later
--  on.
--
--  Third, you have to explicitly assign a subroutine to @main@ to specify which
--  subroutine to run when your program begins.  This is because Haskell lets
--  you define things out of order.  For example, we could have written our
--  original program this way instead:
--
-- >#!/usr/bin/env runhaskell
-- >
-- >{-# LANGUAGE OverloadedStrings #-}
-- >
-- >import Turtle
-- >
-- >main = echo str
-- >
-- >str = "Hello, world!"
--
--  Notice how the above program defines @str@ after @main@, which is completely
--  valid Haskell code.  The order in which you define things (using @=@) does
--  not matter.
--
--  The top level of a Haskell program only permits definitions.  For example,
--  if you were to insert a statement at the top-level:
--
-- >#!/usr/bin/env runhaskell
-- >
-- >{-# LANGUAGE OverloadedStrings #-}
-- >
-- >import Turtle
-- >
-- >echo "Hello, world!"
--
--  ... then you would get this error when you tried to run your program:
--
-- > example.hs:7:1: Parse error: naked expression at top level

-- $do
--  Let's try to execute more than one command this time.  The following script
--  prints the current working directory:
--
-- >#!/usr/bin/env runhaskell
-- >
-- >{-# LANGUAGE OverloadedStrings #-}
-- >
-- >import Turtle
-- >
-- >main = do
-- >    dir <- pwd
-- >    echo dir
