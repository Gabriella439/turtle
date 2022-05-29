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
      Format (..)
    , (%)
    , format
    , printf
    , eprintf
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
    , l
    , fp
    , utc

    -- * Utilities
    , repr
    ) where

import Control.Category (Category(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text, pack)
import Data.Time (UTCTime)
import Data.Word
import Numeric (showEFloat, showFFloat, showGFloat, showHex, showOct)
import Prelude hiding ((.), id)
import qualified System.IO as IO
import Turtle.Line (Line)

import qualified Data.Text.IO as Text
import qualified Turtle.Line

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

{-| Print a `Format` string to standard output (without a trailing newline)

>>> printf ("Hello, "%s%"!\n") "world"
Hello, world!
-}
printf :: MonadIO io => Format (io ()) r -> r
printf fmt = fmt >>- (liftIO . Text.putStr)

{-| Print a `Format` string to standard err (without a trailing newline)

>>> eprintf ("Hello, "%s%"!\n") "world"
Hello, world!
-}
eprintf :: MonadIO io => Format (io ()) r -> r
eprintf fmt = fmt >>- (liftIO . Text.hPutStr IO.stderr)

-- | Create your own format specifier
makeFormat :: (a -> Text) -> Format r (a -> r)
makeFormat k = Format (\return_ -> \a -> return_ (k a))

{-| `Format` any `Show`able value

>>> format w True
"True"
-}
w :: Show a => Format r (a -> r)
w = makeFormat (pack . show)

{-| `Format` an `Integral` value as a signed decimal

>>> format d 25
"25"
>>> format d (-25)
"-25"
-}
d :: Integral n => Format r (n -> r)
d = makeFormat (pack . show . toInteger)

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

{-| `Format` that inserts a `Line`

>>> format l "ABC"
"ABC"
-}
l :: Format r (Line -> r)
l = makeFormat Turtle.Line.lineToText

-- | `Format` a `FilePath` into `Text`
fp :: Format r (FilePath -> r)
fp = makeFormat pack

-- | `Format` a `UTCTime` into `Text`
utc :: Format r (UTCTime -> r)
utc = w

{-| Convert a `Show`able value to any type that implements `IsString` (such as
    `Text`)

>>> repr (1,2)
"(1,2)"
-}
repr :: (Show a, IsString text) => a -> text
repr = fromString . show
