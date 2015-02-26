{-# LANGUAGE OverloadedStrings #-}

{-| Minimalist implementation of type-safe formatted strings, borrowing heavily
    from the implementation of the @formatting@ package.

    Example use of this module:

>>> :set -XOverloadedStrings
>>> import Turtle.Format
>>> format ("This is a "%s%" string that takes "%d%" arguments") "format" 2
"This is a format string that takes 2 arguments"

    A `Format` string that takes no arguments has this type:

> "I take 0 arguments" :: Format r r
>
> format "I take 0 arguments" :: Text

>>> format "I take 0 arguments"
"I take 0 arguments"

    A `Format` string that takes one argument has this type:

> "I take "%d%" arguments" :: Format r (Int -> r)
>
> format ("I take "%d%" argument") :: Int -> Text

>>> format ("I take "%d%" argument") 1
"I take 1 argument"

    A `Format` string that takes two arguments has this type:

> "I "%s%" "%d%" arguments" :: Format r (Text -> Int -> r)
>
> format ("I "%s%" "%d%" arguments") :: Text -> Int -> Text

>>> format ("I "%s%" "%d%" arguments") "take" 2
"I take 2 arguments"
-}

{-# LANGUAGE TypeFamilies #-}

module Turtle.Format (
    -- * Format
      Format
    , (%)
    , format
    , makeFormat

    -- * Parameters
    , w
    , d
    , u
    , o
    , x
    , f
    , e
    , g
    , s
    , fp

    -- * Utilities
    , repr
    ) where

import Control.Category (Category(..))
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text, pack)
import Data.Word (Word)
import Filesystem.Path.CurrentOS (FilePath, toText)
import Numeric (showEFloat, showFFloat, showGFloat, showHex, showOct)
import Prelude hiding ((.), id, FilePath)

-- | A `Format` string
newtype Format a b = Format { (>>-) :: (Text -> a) -> b }

instance Category Format where
    id = Format (\return_ -> return_ "")

    fmt1 . fmt2 = Format (\return_ ->
        fmt1 >>- \str1 ->
        fmt2 >>- \str2 ->
        return_ (str1 <> str2) )

-- | Concatenate two `Format` strings
(%) :: Format b c -> Format a b -> Format a c
(%) = (.)

instance (a ~ b) => IsString (Format a b) where
    fromString str = Format (\return_ -> return_ (pack str))

{-| Convert a `Format` string to a print function that takes zero or more typed
    arguments and returns a `Text` string
-}
format :: Format Text r -> r
format fmt = fmt >>- id

-- | Create your own format specifier
makeFormat :: (a -> Text) -> Format r (a -> r)
makeFormat k = Format (\return_ -> \a -> return_ (k a))

{-| `Format` any `Show`able value

>>> format w True
"True"
-}
w :: Show a => Format r (a -> r)
w = makeFormat (pack . show)

{-| `Format` an `Int` value as a signed decimal

>>> format d 25
"25"
>>> format d (-25)
"-25"
-}
d :: Format r (Int -> r)
d = w

{-| `Format` a `Word` value as an unsigned decimal

>>> format u 25
"25"
-}
u :: Format r (Word -> r)
u = w

{-| `Format` a `Word` value as an unsigned octal number

>>> format o 25
"31"
-}
o :: Format r (Word -> r)
o = makeFormat (\n -> pack (showOct n ""))

{-| `Format` a `Word` value as an unsigned hexadecimal number (without a
    leading \"0x\")

>>> format x 25
"19"
-}
x :: Format r (Word -> r)
x = makeFormat (\n -> pack (showHex n ""))

{-| `Format` a `Double` using decimal notation with 6 digits of precision

>>> format f 25.1
"25.100000"
-}
f :: Format r (Double -> r)
f = makeFormat (\n -> pack (showFFloat (Just 6) n ""))

{-| `Format` a `Double` using scientific notation with 6 digits of precision

>>> format e 25.1
"2.510000e1"
-}
e :: Format r (Double -> r)
e = makeFormat (\n -> pack (showEFloat (Just 6) n ""))

{-| `Format` a `Double` using decimal notation for small exponents and
    scientific notation for large exponents

>>> format g 25.1
"25.100000"
>>> format g 123456789
"1.234568e8"
>>> format g 0.00000000001
"1.000000e-11"
-}
g :: Format r (Double -> r)
g = makeFormat (\n -> pack (showGFloat (Just 6) n ""))

{-| `Format` that inserts `Text`

>>> format s "ABC"
"ABC"
-}
s :: Format r (Text -> r)
s = makeFormat id

{-| `Format` a `Filesystem.Path.CurrentOS.FilePath` into `Text`
-}
fp :: Format r (FilePath -> r)
fp = makeFormat (\fpath -> either id id (toText fpath))

{-| Convert a `Show`able value to `Text`

    Short-hand for @(format w)@

>>> repr (1,2)
"(1,2)"
-}
repr :: Show a => a -> Text
repr = format w
