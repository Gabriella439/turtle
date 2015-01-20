{-# LANGUAGE OverloadedStrings #-}

{-| Minimalist implementation of type-safe formatted strings, borrowing heavily
    from the implementation of the @formatting@ package.

    Example use of this module:

>>> :set -XOverloadedStrings
>>> import Turtle
>>> format ("This is a "%s%" string that takes "%d%" arguments") "format" 2
"This is a format string that takes 2 arguments"

    A `Format` string that takes no arguments has this type:

> "I take 0 arguments" :: Format r r
>
> format "I take 0 arguments" :: Text

    A `Format` string that takes one argument has this type:

> "I take "%d%" arguments" :: Format r (Int -> r)
>
> format ("I take "%d%" argument") :: Int -> Text
>
> format ("I take "%d%" argument") 1 :: Text

    A `Format` string that takes two arguments has this type:

> "I "%s%" "%d%" arguments" :: Format r (Text -> Int -> r)
>
> format ("I "%s%" "%d%" arguments") :: Text -> Int -> Text
>
> format ("I "%s%" "%d%" arguments") "take" 2 :: Text
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
    , f
    , s
    ) where

import Control.Category (Category(..))
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Data.Text (Text, pack)
import Data.Word (Word)
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

-- | `Format` string that inserts any `Show`able value
w :: Show a => Format r (a -> r)
w = makeFormat (pack . show)

-- | `Format` string that inserts an `Int` value
d :: Format r (Int -> r)
d = w

-- | `Format` string that inserts a `Word` value
u :: Format r (Word -> r)
u = w

-- | `Format` string that inserts a `Double`
f :: Format r (Double -> r)
f = w

-- | `Format` string that inserts `Text`
s :: Format r (Text -> r)
s = makeFormat id
