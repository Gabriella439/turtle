{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-| Use this module to either:

    * match `Text` with light-weight backtracking patterns, or:

    * parse structured values from `Text`.

    Example usage:

>>> :set -XOverloadedStrings
>>> match ("can" <|> "cat") "cat"
["cat"]
>>> match ("can" <|> "cat") "dog"
[]
>>> match (decimal `sepBy` ",") "1,2,3"
[[1,2,3]]

    This pattern has unlimited backtracking, and will return as many solutions
    as possible:

>>> match (prefix (star anyChar)) "123"
["123","12","1",""]

    Use @do@ notation to structure more complex patterns:

>>> :{
let bit = ("0" *> pure False) <|> ("1" *> pure True) :: Pattern Bool;
    portableBitMap = do
        { "P1"
        ; width  <- spaces1 *> decimal
        ; height <- spaces1 *> decimal
        ; count width (count height (spaces1 *> bit))
        };
in  match (prefix portableBitMap) "P1\n2 2\n0 0\n1 0\n"
:}
[[[False,False],[True,False]]]

-}

module Turtle.Pattern (
    -- * Pattern
      Pattern
    , match

    -- * Primitive patterns
    , anyChar
    , eof

    -- * Character patterns
    , dot
    , satisfy
    , char
    , notChar
    , text
    , oneOf
    , noneOf
    , space
    , spaces
    , spaces1
    , tab
    , newline
    , crlf
    , upper
    , lower
    , alphaNum
    , letter
    , digit
    , hexDigit
    , octDigit

    -- * Numbers
    , decimal
    , signed

    -- * Combinators
    , prefix
    , suffix
    , has
    , once
    , star
    , plus
    , selfless
    , choice
    , count
    , option
    , between
    , skip
    , within
    , fixed
    , sepBy
    , sepBy1
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.List (foldl')
import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text

-- | A fully backtracking pattern that parses an @\'a\'@ from some `Text`
newtype Pattern a = Pattern { runPattern :: StateT Text [] a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance Monoid a => Monoid (Pattern a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance Num a => Num (Pattern a) where
    fromInteger n = pure (fromInteger n)

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

    abs    = fmap abs
    signum = fmap signum
    negate = fmap negate

instance Fractional a => Fractional (Pattern a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance Floating a => Floating (Pattern a) where
    pi = pure pi

    exp   = fmap exp
    sqrt  = fmap sqrt
    log   = fmap log
    sin   = fmap sin
    tan   = fmap tan
    cos   = fmap cos
    asin  = fmap sin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    tanh  = fmap tanh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

    (**)    = liftA2 (**)
    logBase = liftA2 logBase

instance (a ~ Text) => IsString (Pattern a) where
    fromString str = text (Text.pack str)

{-| Match a `Pattern` against a `Text` input, returning all possible solutions

    The `Pattern` must match the entire `Text`
-}
match :: Pattern a -> Text -> [a]
match p = evalStateT (runPattern (p <* eof))

{-| Match any character

>>> match anyChar "1"
"1"
>>> match anyChar ""
""
-}
anyChar :: Pattern Char
anyChar = Pattern (do
    Just (c, cs) <- fmap Text.uncons get
    put cs
    return c )

{-| Matches the end of input

>>> match eof "1"
[]
>>> match eof ""
[()]
-}
eof :: Pattern ()
eof = Pattern (do
    True <- fmap Text.null get
    return () )

-- | Synonym for `anyChar`
dot :: Pattern Char
dot = anyChar

{-| Match any character that satisfies the given predicate

>>> match (satisfy (== '1')) "1"
"1"
>>> match (satisfy (== '2')) "1"
""
-}
satisfy :: (Char -> Bool) -> Pattern Char
satisfy predicate = do
    c <- anyChar
    guard (predicate c)
    return c

{-| Match a specific character

>>> match (char '1') "1"
"1"
>>> match (char '2') "1"
""
-}
char :: Char -> Pattern Char
char c = satisfy (== c)

{-| Match any character except the given one

>>> match (notChar '2') "1"
"1"
>>> match (notChar '1') "1"
""
-}
notChar :: Char -> Pattern Char
notChar c = satisfy (/= c)

{-| Match a specific string

>>> match (text "123") "123"
["123"]

    You can also omit the `text` function if you enable the @OverloadedStrings@
    extension:

>>> match "123" "123"
["123"]
-}
text :: Text -> Pattern Text
text before' = Pattern (do
    txt <- get
    let (before, after) = Text.splitAt (Text.length before') txt
    guard (before == before')
    put after
    return before)

{-| Match any one of the given characters

>>> match (oneOf "1a") "1"
"1"
>>> match (oneOf "2a") "1"
""
-}
oneOf :: [Char] -> Pattern Char
oneOf cs = satisfy (`elem` cs)

{-| Match anything other than the given characters

>>> match (noneOf "2a") "1"
"1"
>>> match (noneOf "1a") "1"
""
-}
noneOf :: [Char] -> Pattern Char
noneOf cs = satisfy (`notElem` cs)

{-| Match a whitespace character

>>> match space " "
" "
>>> match space "1"
""
-}
space :: Pattern Char
space = satisfy isSpace

{-| Match zero or more whitespace characters

>>> match spaces "  "
["  "]
>>> match spaces ""
[""]
-}
spaces :: Pattern Text
spaces = star space

{-| Match one or more whitespace characters

>>> match spaces1 "  "
["  "]
>>> match spaces1 ""
[]
-}
spaces1 :: Pattern Text
spaces1 = plus space

{-| Match the tab character (@\'\t\'@)

>>> match tab "\t"
"\t"
>>> match tab " "
""
-}
tab :: Pattern Char
tab = char '\t'

{-| Match the newline character (@\'\n\'@)

>>> match newline "\n"
"\n"
>>> match newline " "
""
-}
newline :: Pattern Char
newline = char '\n'

{-| Matches a carriage return (@\'\r\'@) followed by a newline (@\'\n\'@)

>>> match crlf "\r\n"
["\r\n"]
>>> match crlf "\n\r"
[]
-}
crlf :: Pattern Text
crlf = text "\r\n"

{-| Match an uppercase letter

>>> match upper "A"
"A"
>>> match upper "a"
""
-}
upper :: Pattern Char
upper = satisfy isUpper

{-| Match a lowercase letter

>>> match lower "a"
"a"
>>> match lower "A"
""
-}
lower :: Pattern Char
lower = satisfy isLower

{-| Match a letter or digit

>>> match alphaNum "1"
"1"
>>> match alphaNum "a"
"a"
>>> match alphaNum "A"
"A"
>>> match alphaNum "."
""
-}
alphaNum :: Pattern Char
alphaNum = satisfy isAlphaNum

{-| Match a letter

>>> match letter "A"
"A"
>>> match letter "a"
"a"
>>> match letter "1"
""
-}
letter :: Pattern Char
letter = satisfy isLetter

{-| Match a digit

>>> match digit "1"
"1"
>>> match digit "a"
""
-}
digit :: Pattern Char
digit = satisfy isDigit

{-| Match a hexadecimal digit

>>> match hexDigit "1"
"1"
>>> match hexDigit "A"
"A"
>>> match hexDigit "a"
"a"
>>> match hexDigit "g"
""
-}
hexDigit :: Pattern Char
hexDigit = satisfy isHexDigit

{-| Match an octal digit

>>> match octDigit "1"
"1"
>>> match octDigit "9"
""
-}
octDigit :: Pattern Char
octDigit = satisfy isOctDigit

{-| Match an unsigned decimal number

>>> match decimal  "123"
[123]
>>> match decimal "-123"
[]
-}
decimal :: Num n => Pattern n
decimal = do
    ds <- some digit
    return (foldl' step 0 ds)
  where
    step n d = n * 10 + fromIntegral (ord d - ord '0')

{-| Transform a numeric parser to accept an optional leading @\'+\'@ or @\'-\'@
    sign

>>> match (signed decimal) "+123"
[123]
>>> match (signed decimal) "-123"
[-123]
>>> match (signed decimal)  "123"
[123]
-}
signed :: Num a => Pattern a -> Pattern a
signed p = do
    sign <- (char '+' *> pure id) <|> (char '-' *> pure negate) <|> (pure id)
    fmap sign p

{-| Match a `Char`, but return `Text`

>>> match (once (char '1')) "1"
["1"]
>>> match (once (char '1')) ""
[]
-}
once :: Pattern Char -> Pattern Text
once p = fmap Text.singleton p

{-| Use this to match the prefix of a string

>>> match         "A"  "ABC"
[]
>>> match (prefix "A") "ABC"
["A"]
-}
prefix :: Pattern a -> Pattern a
prefix p = p <* star anyChar

{-| Use this to match the suffix of a string

>>> match         "C"  "ABC"
[]
>>> match (suffix "C") "ABC"
["C"]
-}
suffix :: Pattern a -> Pattern a
suffix p = star anyChar *> p

{-| Use this to match the interior of a string

>>> match      "B"  "ABC"
[]
>>> match (has "B") "ABC"
["B"]
-}
has :: Pattern a -> Pattern a
has p = star anyChar *> p <* star anyChar

{-| Parse 0 or more occurrences of the given character

>>> match (star anyChar) "123"
["123"]
>>> match (star anyChar) ""
[""]
-}
star :: Pattern Char -> Pattern Text
star p = fmap Text.pack (many p)

{-| Parse 1 or more occurrences of the given character

>>> match (plus anyChar) "123"
["123"]
>>> match (plus anyChar) ""
[]
-}
plus :: Pattern Char -> Pattern Text
plus p = fmap Text.pack (some p)

{-| Patterns that match multiple times are greedy by default, meaning that they
    try to match as many times as possible.  The `selfless` combinator makes a
    pattern match as few times as possible

    This only changes the order in which solutions are returned, by prioritizing
    less greedy solutions

>>> match (prefix (selfless (some anyChar))) "123"
["1","12","123"]
>>> match (prefix           (some anyChar) ) "123"
["123","12","1"]
-}
selfless :: Pattern a -> Pattern a
selfless p = Pattern (StateT (\s -> reverse (runStateT (runPattern p) s)))

{-| Apply the patterns in the list in order, until one of them succeeds

>>> match (choice ["cat", "dog", "egg"]) "egg"
["egg"]
>>> match (choice ["cat", "dog", "egg"]) "cat"
["cat"]
>>> match (choice ["cat", "dog", "egg"]) "fan"
[]
-}
choice :: [Pattern a] -> Pattern a
choice = msum

{-| Apply the given pattern a fixed number of times, collecting the results

>>> match (count 3 anyChar) "123"
["123"]
>>> match (count 4 anyChar) "123"
[]
-}
count :: Int -> Pattern a -> Pattern [a]
count = replicateM

{-| Transform a parser to a succeed with an empty value instead of failing

    See also: `optional`

>>> match (option "1" <> "2") "12"
["12"]
>>> match (option "1" <> "2") "2"
["2"]
-}
option :: Monoid a => Pattern a -> Pattern a
option p = p <|> mempty

{-| @(between open close p)@ matches @\'p\'@ in between @\'open\'@ and
    @\'close\'@

>>> match (between (char '(') (char ')') (star anyChar)) "(123)"
["123"]
>>> match (between (char '(') (char ')') (star anyChar)) "(123"
[]
-}
between :: Pattern a -> Pattern b -> Pattern c -> Pattern c
between open close p = open *> p <* close

{-| Discard the pattern's result

>>> match (skip anyChar) "1"
[()]
>>> match (skip anyChar) ""
[]
-}
skip :: Pattern a -> Pattern ()
skip = void

{-| Restrict the pattern to consume no more than the given number of characters

>>> match (within 2 decimal) "12"
[12]
>>> match (within 2 decimal) "1"
[1]
>>> match (within 2 decimal) "123"
[]
-}
within :: Int -> Pattern a -> Pattern a
within n p = Pattern (do
    txt <- get
    let (before, after) = Text.splitAt n txt
    put before
    a <- runPattern p
    modify (<> after)
    return a )

{-| Require the pattern to consume exactly the given number of characters

>>> match (fixed 2 decimal) "12"
[12]
>>> match (fixed 2 decimal) "1"
[]
-}
fixed :: Int -> Pattern a -> Pattern a
fixed n p = do
    txt <- Pattern get
    guard (Text.length txt >= n)
    within n (p <* eof)

{-| @p `sepBy` sep@ matches zero or more occurrences of @p@ separated by @sep@

>>> match (decimal `sepBy` char ',') "1,2,3"
[[1,2,3]]
>>> match (decimal `sepBy` char ',') ""
[[]]
-}
sepBy :: Pattern a -> Pattern b -> Pattern [a]
p `sepBy` sep = (p `sepBy1` sep) <|> pure []

{-| @p `sepBy1` sep@ matches one or more occurrences of @p@ separated by @sep@

>>> match (decimal `sepBy1` ",") "1,2,3"
[[1,2,3]]
>>> match (decimal `sepBy1` ",") ""
[]
-}
sepBy1 :: Pattern a -> Pattern b -> Pattern [a]
p `sepBy1` sep = (:) <$> p <*> many (sep *> p) 
