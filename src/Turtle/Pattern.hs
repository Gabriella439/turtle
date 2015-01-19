{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-| A light-weight backtracking pattern

    Example usage:

>>> :set -XOverloadedStrings
>>> prefix ("dog" <|> "cat") "cat"
["cat"]
>>> prefix (plus (notChar ',') <* char ',') "cat,dog"
["cat"]
>>> prefix (count 3 anyChar) "cat,dog"
["cat"]

    This pattern has unlimited backtracking, and will return as many solutions
    as possible:

>>> prefix (plus anyChar) "123"
["123","12","1"]
>>> prefix (plus anyChar <* eof) "123"
["123"]

    Use @do@ notation to structure more complex patterns:

>>> :{
let bit = (char '0' *> pure False) <|> (char '1' *> pure True);
    portableBitMap = do
        { text "P1"
        ; width  <- spaces1 *> decimal
        ; height <- spaces1 *> decimal
        ; count width (count height (spaces1 *> bit))
        };
in  prefix portableBitMap "P1\n2 2\n0 0\n1 0\n"
:}
[[[False,False],[True,False]]]

-}

module Turtle.Pattern (
    -- * Pattern
      Pattern
    , match
    , prefix
    , inside
    , suffix

    -- * Primitive patterns
    , anyChar
    , eof

    -- * Character patterns
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
    , star
    , plus
    , selfless
    , choice
    , count
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

-- | A backtracking pattern
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

{-| Match a `Pattern` against a `Text` input, returning all possible matches

    The `Pattern` must match the entire `Text`
-}
match :: Pattern a -> Text -> [a]
match p = prefix (p <* eof)

{-| Match a `Pattern` against a `Text` input, returning all possible matches

    The `Pattern` must match some prefix of the `Text`
-}
prefix :: Pattern a -> Text -> [a]
prefix p = evalStateT (runPattern p)

{-| Match a `Pattern` against a `Text` input, returning all possible matches

    The `Pattern` must match some suffix of the `Text`
-}
suffix :: Pattern a -> Text -> [a]
suffix p = inside (p <* eof)

{-| Match a `Pattern` against a `Text` input, returning all possible matches

    The `Pattern` can match any subset of the `Text`
-}
inside :: Pattern a -> Text -> [a]
inside p = prefix (selfless (star anyChar) *> p)

{-| Match any character

>>> prefix anyChar "123"
"1"
>>> prefix anyChar ""
""

-}
anyChar :: Pattern Char
anyChar = Pattern (do
    Just (c, cs) <- fmap Text.uncons get
    put cs
    return c )

{-| Matches the end of input

>>> prefix eof "123"
[]
>>> prefix eof ""
[()]

-}
eof :: Pattern ()
eof = Pattern (do
    True <- fmap Text.null get
    return () )

{-| Match any character that satisfies the given predicate

>>> prefix (satisfy (== '1')) "123"
"1"
>>> prefix (satisfy (== '2')) "123"
""

-}
satisfy :: (Char -> Bool) -> Pattern Char
satisfy predicate = do
    c <- anyChar
    guard (predicate c)
    return c

{-| Match a specific character

>>> prefix (char '1') "123"
"1"
>>> prefix (char '2') "123"
""

-}
char :: Char -> Pattern Char
char c = satisfy (== c)

{-| Match any character except the given one

>>> prefix (notChar '2') "123"
"1"
>>> prefix (notChar '1') "123"
""

-}
notChar :: Char -> Pattern Char
notChar c = satisfy (/= c)

{-| Match a specific string

>>> prefix (text "12") "123"
["12"]

    You can also omit the `text` function if you enable the @OverloadedStrings@
    extension:

>>> prefix "12" "123"
["12"]

-}
text :: Text -> Pattern Text
text before' = Pattern (do
    txt <- get
    let (before, after) = Text.splitAt (Text.length before') txt
    guard (before == after)
    put after
    return before)

{-| Match any one of the given characters

>>> prefix (oneOf "1a") "123"
"1"
>>> prefix (oneOf "2a") "123"
""

-}
oneOf :: [Char] -> Pattern Char
oneOf cs = satisfy (`elem` cs)

{-| Match anything other than the given characters

>>> prefix (noneOf "2a") "123"
"1"
>>> prefix (noneOf "1a") "123"
""

-}
noneOf :: [Char] -> Pattern Char
noneOf cs = satisfy (`notElem` cs)

{-| Match a whitespace character

>>> prefix space " a"
" "
>>> prefix space "a "
""

-}
space :: Pattern Char
space = satisfy isSpace

{-| Match zero or more whitespace characters

>>> prefix spaces "  "
["  "," ",""]
>>> prefix spaces "a "
[""]

-}
spaces :: Pattern Text
spaces = star space

{-| Match one or more whitespace characters

>>> prefix spaces1 "  "
["  "," "]
>>> prefix spaces1 "a "
[]

-}
spaces1 :: Pattern Text
spaces1 = plus space

{-| Match the tab character (@\'\t\'@)

>>> prefix tab "\t"
"\t"
>>> prefix tab " "
""
-}
tab :: Pattern Char
tab = char '\t'

{-| Match the newline character (@\'\n\'@)

>>> prefix newline "\n"
"\n"
>>> prefix newline " "
""
-}
newline :: Pattern Char
newline = char '\n'

{-| Matches a carriage return (@\'\r\'@) followed by a newline (@\'\n\'@)

>>> prefix crlf "\r\n"
["\r\n"]
>>> prefix crlf "\n\r"
[]
-}
crlf :: Pattern Text
crlf = text "\r\n"

{-| Match an uppercase letter

>>> prefix upper "ABC"
"A"
>>> prefix upper "abc"
""
-}
upper :: Pattern Char
upper = satisfy isUpper

{-| Match a lowercase letter

>>> prefix lower "abc"
"a"
>>> prefix lower "ABC"
""
-}
lower :: Pattern Char
lower = satisfy isLower

{-| Match a letter or digit

>>> prefix alphaNum "123"
"1"
>>> prefix alphaNum "abc"
"a"
>>> prefix alphaNum "ABC"
"A"
>>> prefix alphaNum "..."
""
-}
alphaNum :: Pattern Char
alphaNum = satisfy isAlphaNum

{-| Match a letter

>>> prefix letter "ABC"
"A"
>>> prefix letter "abc"
"a"
>>> prefix letter "123"
""
-}
letter :: Pattern Char
letter = satisfy isLetter

{-| Match a digit

>>> prefix digit "123"
"1"
>>> prefix digit "abc"
""
-}
digit :: Pattern Char
digit = satisfy isDigit

{-| Match a hexadecimal digit

>>> prefix hexDigit "123"
"1"
>>> prefix hexDigit "ABC"
"A"
>>> prefix hexDigit "abc"
"a"
>>> prefix hexDigit "ghi"
""
-}
hexDigit :: Pattern Char
hexDigit = satisfy isHexDigit

{-| Match an octal digit

>>> prefix octDigit "123"
"1"
>>> prefix octDigit "9"
""
-}
octDigit :: Pattern Char
octDigit = satisfy isOctDigit

{-| Match an unsigned decimal number

>>> prefix (decimal <* eof) "123"
[123]
>>> prefix (decimal <* eof) "-123"
[]
-}
decimal :: Num n => Pattern n
decimal = do
    ds <- some digit
    return (foldl' step 0 ds)
  where
    step n d = n * 10 + fromIntegral (ord d - ord '0')

{-| Parse a number with an optional leading @\'+\'@ or @\'-\'@ sign

>>> prefix (signed decimal <* eof) "+123"
[123]
>>> prefix (signed decimal <* eof) "-123"
[-123]
>>> prefix (signed decimal <* eof)  "123"
[123]
-}
signed :: Num a => Pattern a -> Pattern a
signed p = do
    sign <- (char '+' *> pure id) <|> (char '-' *> pure negate) <|> (pure id)
    fmap sign p

{-| Pattern 0 or more occurrecnes of the given character

>>> prefix (star anyChar) "123"
["123","12","1",""]
>>> prefix (star anyChar <* eof) "123"
["123"]
>>> prefix (star anyChar) ""
[""]
-}
star :: Pattern Char -> Pattern Text
star p = fmap Text.pack (many p)

{-| Patterns that match multiple times are greedy by default, meaning that they
    try to match as many times as possible.  The `selfless` combinator makes a
    pattern match as few times as possible

>>> prefix (selfless (star anyChar) *> char '1') "123"
"1"
>>> prefix (selfless (star anyChar)) "123"
["","1","12","123"]
>>> prefix (selfless (star anyChar <* eof)) "123"
["123"]
-}
selfless :: Pattern a -> Pattern a
selfless p = Pattern (StateT (\s -> reverse (runStateT (runPattern p) s)))

{-| Parse 1 or more occurrences of the given character

>>> prefix (plus anyChar) "123"
["123","12","1"]
>>> prefix (plus anyChar <* eof) "123"
["123"]
>>> prefix (plus anyChar) ""
[]
-}
plus :: Pattern Char -> Pattern Text
plus p = fmap Text.pack (some p)

{-| Apply the patterns in the list in order, until one of them succeeds

>>> prefix (choice [text "cat", text "dog", text "egg"]) "egg"
["egg"]
>>> prefix (choice [text "cat", text "dog", text "egg"]) "cat"
["cat"]
>>> prefix (choice [text "cat", text "dog", text "egg"]) "fan"
[]
-}
choice :: [Pattern a] -> Pattern a
choice = msum

{-| Apply the given pattern a fixed number of times, collecting the results

>>> prefix (count 2 anyChar) "123"
["12"]
>>> prefix (count 4 anyChar) "123"
[]
-}
count :: Int -> Pattern a -> Pattern [a]
count = replicateM

{-| @between open close p@ matches @p@ in between @open@ and @close@

>>> prefix (between (char '(') (char ')') (star anyChar)) "(123)"
["123"]
>>> prefix (between (char '(') (char ')') (star anyChar)) "(123))"
["123)","123"]
>>> prefix (between (char '(') (char ')') (star anyChar)) "(123"
[]
-}
between :: Pattern a -> Pattern b -> Pattern c -> Pattern c
between open close p = open *> p <* close

{-| Discard the pattern's result

>>> prefix (skip anyChar) "123"
[()]
>>> prefix (skip anyChar) ""
[]
-}
skip :: Pattern a -> Pattern ()
skip = void

{-| Restrict the pattern to consume no more than the given number of characters

>>> prefix (within 2 decimal) "123"
[12,1]
>>> prefix (within 2 decimal) "1a3"
[1]
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

>>> prefix (fixed 2 decimal) "123"
[12]
>>> prefix (fixed 2 decimal) "1a3"
[]
-}
fixed :: Int -> Pattern a -> Pattern a
fixed n p = within n (p <* eof)

{-| @p `sepBy` sep@ matches zero or more occurrences of @p@ separated by @sep@

>>> prefix (decimal `sepBy` char ',') "1,2,3"
[[1,2,3],[1,2],[1],[]]
>>> prefix (decimal `sepBy` char ',' <* eof) "1,2,3"
[[1,2,3]]
>>> prefix (decimal `sepBy` char ',') ""
[[]]
-}
sepBy :: Pattern a -> Pattern b -> Pattern [a]
p `sepBy` sep = (p `sepBy1` sep) <|> pure []

{-| @p `sepBy1` sep@ matches one or more occurrences of @p@ separated by @sep@

>>> prefix (decimal `sepBy1` char ',') "1,2,3"
[[1,2,3],[1,2],[1]]
>>> prefix (decimal `sepBy1` char ',' <* eof) "1,2,3"
[[1,2,3]]
>>> prefix (decimal `sepBy1` char ',') ""
[]
-}
sepBy1 :: Pattern a -> Pattern b -> Pattern [a]
p `sepBy1` sep = (:) <$> p <*> many (sep *> p) 
