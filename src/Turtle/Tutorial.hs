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
-}

module Turtle.Tutorial (
    -- * Introduction
    -- $introduction

    -- * Comparison - Part 1
    -- $compare1
    ) where

import Turtle

-- $introduction
--  Let's translate some simple Bash scripts to Haskell and work our way up to
--  more complex scripts:
--
--  This is how you write a \"Hello, world\" script in both languages:
--
-- > #!/usr/bin/env runhaskell           -- #!/bin/bash
-- >                                     --
-- > {-# LANGUAGE OverloadedStrings #-}  --
-- >                                     --
-- > import Turtle                       --
-- >                                     --
-- > main = echo "Hello, world!"         -- echo Hello, world!
--
--  You can test the above code by saving it to the file @example00.hs@, making
--  the file executable, and then running the file:
--
-- > $ chmod u+x example00.hs 
-- > $ ./example00.hs
-- > Hello, world!
--
--  If you delete the first line of the program, you can also compile the above
--  code to generate a native executable:
--
-- > $ ghc -O2 example00.hs
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
--  and everything else.  Haskell actually does let you blur the line, too, but
--  in a more limited way, and we will see examples of that later on.
--
--  Third, you have to explicitly assign a subroutine to @main@ to specify which
--  subroutine to run when your program begins.  This is because Haskell lets
--  you define things out of order.  For example, we could have written our
--  original program this way instead:
--
-- > #!/usr/bin/env runhaskell
-- >
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Turtle
-- >
-- > main = echo str
-- >
-- > str = "Hello World"
--
--  Notice how the above program defines @str@ after @main@, which is completely
--  valid Haskell code.  The order in which you define things (using @=@) does
--  not matter.
