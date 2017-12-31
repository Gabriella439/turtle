{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Turtle.Line
  ( Line
  , lineToText
  , textToLines
  , linesToText
  , textToLine
  , unsafeTextToLine
  , NewlineForbidden(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#endif
import Data.List.NonEmpty (NonEmpty(..))
import Data.String
#if __GLASGOW_HASKELL__ >= 710
#else
import Data.Monoid
#endif
import Data.Maybe
import Data.Typeable
import Control.Exception

import qualified Data.List.NonEmpty

-- | The `NewlineForbidden` exception is thrown when you construct a `Line`
-- using an overloaded string literal or by calling `fromString` explicitly
-- and the supplied string contains newlines. This is a programming error to
-- do so: if you aren't sure that the input string is newline-free, do not
-- rely on the @`IsString` `Line`@ instance.
--
-- When debugging, it might be useful to look for implicit invocations of
-- `fromString` for `Line`:
--
-- > >>> sh (do { line <- "Hello\nWorld"; echo line })
-- > *** Exception: NewlineForbidden
--
-- In the above example, `echo` expects its argument to be a `Line`, thus
-- @line :: `Line`@. Since we bind @line@ in `Shell`, the string literal
-- @\"Hello\\nWorld\"@ has type @`Shell` `Line`@. The
-- @`IsString` (`Shell` `Line`)@ instance delegates the construction of a
-- `Line` to the @`IsString` `Line`@ instance, where the exception is thrown.
--
-- To fix the problem, use `textToLines`:
--
-- > >>> sh (do { line <- select (textToLines "Hello\nWorld"); echo line })
-- > Hello
-- > World
data NewlineForbidden = NewlineForbidden
  deriving (Show, Typeable)

instance Exception NewlineForbidden

-- | A line of text (does not contain newlines).
newtype Line = Line Text
  deriving (Eq, Ord, Show, Monoid)

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup Line where
  (<>) = mappend
#endif

instance IsString Line where
  fromString = fromMaybe (throw NewlineForbidden) . textToLine . fromString

-- | Convert a line to a text value.
lineToText :: Line -> Text
lineToText (Line t) = t

-- | Split text into lines. The inverse of `linesToText`.
textToLines :: Text -> NonEmpty Line
textToLines =
#if __GLASGOW_HASKELL__ >= 708
  Data.List.NonEmpty.fromList . coerce (Text.splitOn "\n")
#else
  Data.List.NonEmpty.fromList . map unsafeTextToLine . Text.splitOn "\n"
#endif

-- | Merge lines into a single text value.
linesToText :: [Line] -> Text
linesToText =
#if __GLASGOW_HASKELL__ >= 708
  coerce Text.unlines
#else
  Text.unlines . map lineToText
#endif

-- | Try to convert a text value into a line.
-- Precondition (checked): the argument does not contain newlines.
textToLine :: Text -> Maybe Line
textToLine = fromSingleton . textToLines
  where
    fromSingleton (a :| []) = Just a
    fromSingleton  _        = Nothing

-- | Convert a text value into a line.
-- Precondition (unchecked): the argument does not contain newlines.
unsafeTextToLine :: Text -> Line
unsafeTextToLine = Line
