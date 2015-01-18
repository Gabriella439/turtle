{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-| A light-weight backtracking pattern

    Example usage:

>>> :set -XOverloadedStrings
>>> match ("dog" <|> "cat") "cat"
["cat"]
>>> match (plus (notChar ',') <* char ',') "cat,dog"
["cat"]
>>> match (count 3 anyChar) "cat,dog"
["cat"]

    This pattern has unlimited backtracking, and will return as many solutions
    as possible:

>>> match (plus anyChar) "123"
["123","12","1"]
>>> match (plus anyChar <* eof) "123"
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
in  match portableBitMap "P1\n2 2\n0 0\n1 0\n"
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

    -- * Re-exports
    , Text
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

-- | Supply a `Pattern` with a `Text` input, returning all possible matches
match :: Pattern a -> Text -> [a]
match p = evalStateT (runPattern p)

{-| Match any character

>>> match anyChar "123"
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

>>> match eof "123"
[]
>>> match eof ""
[()]

-}
eof :: Pattern ()
eof = Pattern (do
    True <- fmap Text.null get
    return () )

{-| Match any character that satisfies the given predicate

>>> match (satisfy (== '1')) "123"
"1"
>>> match (satisfy (== '2')) "123"
""

-}
satisfy :: (Char -> Bool) -> Pattern Char
satisfy predicate = do
    c <- anyChar
    guard (predicate c)
    return c

{-| Match a specific character

>>> match (char '1') "123"
"1"
>>> match (char '2') "123"
""

-}
char :: Char -> Pattern Char
char c = satisfy (== c)

{-| Match any character except the given one

>>> match (notChar '2') "123"
"1"
>>> match (notChar '1') "123"
""

-}
notChar :: Char -> Pattern Char
notChar c = satisfy (/= c)

{-| Match a specific string

>>> match (text "12") "123"
["12"]

    You can also omit the `text` function if you enable the @OverloadedStrings@
    extension:

>>> match "12" "123"
["12"]

-}
text :: Text -> Pattern Text
text prefix' = Pattern (do
    txt <- get
    let (prefix, suffix) = Text.splitAt (Text.length prefix') txt
    guard (prefix == prefix')
    put suffix
    return prefix )

{-| Match any one of the given characters

>>> match (oneOf "1a") "123"
"1"
>>> match (oneOf "2a") "123"
""

-}
oneOf :: [Char] -> Pattern Char
oneOf cs = satisfy (`elem` cs)

{-| Match anything other than the given characters

>>> match (noneOf "2a") "123"
"1"
>>> match (noneOf "1a") "123"
""

-}
noneOf :: [Char] -> Pattern Char
noneOf cs = satisfy (`notElem` cs)

{-| Match a whitespace character

>>> match space " a"
" "
>>> match space "a "
""

-}
space :: Pattern Char
space = satisfy isSpace

{-| Match zero or more whitespace characters

>>> match spaces "  "
["  "," ",""]
>>> match spaces "a "
[""]

-}
spaces :: Pattern Text
spaces = star space

{-| Match one or more whitespace characters

>>> match spaces1 "  "
["  "," "]
>>> match spaces1 "a "
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

>>> match upper "ABC"
"A"
>>> match upper "abc"
""
-}
upper :: Pattern Char
upper = satisfy isUpper

{-| Match a lowercase letter

>>> match lower "abc"
"a"
>>> match lower "ABC"
""
-}
lower :: Pattern Char
lower = satisfy isLower

{-| Match a letter or digit

>>> match alphaNum "123"
"1"
>>> match alphaNum "abc"
"a"
>>> match alphaNum "ABC"
"A"
>>> match alphaNum "..."
""
-}
alphaNum :: Pattern Char
alphaNum = satisfy isAlphaNum

{-| Match a letter

>>> match letter "ABC"
"A"
>>> match letter "abc"
"a"
>>> match letter "123"
""
-}
letter :: Pattern Char
letter = satisfy isLetter

{-| Match a digit

>>> match digit "123"
"1"
>>> match digit "abc"
""
-}
digit :: Pattern Char
digit = satisfy isDigit

{-| Match a hexadecimal digit

>>> match hexDigit "123"
"1"
>>> match hexDigit "ABC"
"A"
>>> match hexDigit "abc"
"a"
>>> match hexDigit "ghi"
""
-}
hexDigit :: Pattern Char
hexDigit = satisfy isHexDigit

{-| Match an octal digit

>>> match octDigit "123"
"1"
>>> match octDigit "9"
""
-}
octDigit :: Pattern Char
octDigit = satisfy isOctDigit

{-| Match an unsigned decimal number

>>> match (decimal <* eof) "123"
[123]
>>> match (decimal <* eof) "-123"
[]
-}
decimal :: Num n => Pattern n
decimal = do
    ds <- some digit
    return (foldl' step 0 ds)
  where
    step n d = n * 10 + fromIntegral (ord d - ord '0')

{-| Parse a number with an optional leading @\'+\'@ or @\'-\'@ sign

>>> match (signed decimal <* eof) "+123"
[123]
>>> match (signed decimal <* eof) "-123"
[-123]
>>> match (signed decimal <* eof)  "123"
[123]
-}
signed :: Num a => Pattern a -> Pattern a
signed p = do
    sign <- (char '+' *> pure id) <|> (char '-' *> pure negate) <|> (pure id)
    fmap sign p

{-| Pattern 0 or more occurrecnes of the given character

>>> match (star anyChar) "123"
["123","12","1",""]
>>> match (star anyChar <* eof) "123"
["123"]
>>> match (star anyChar) ""
[""]
-}
star :: Pattern Char -> Pattern Text
star p = fmap Text.pack (many p)

{-| Patterns that match multiple times are greedy by default, meaning that they
    try to match as many times as possible.  The `selfless` combinator makes a
    pattern match as few times as possible

>>> match (selfless (star anyChar) *> char '1') "123"
"1"
>>> match (selfless (star anyChar)) "123"
["","1","12","123"]
>>> match (selfless (star anyChar <* eof)) "123"
["123"]
-}
selfless :: Pattern a -> Pattern a
selfless p = Pattern (StateT (\s -> reverse (runStateT (runPattern p) s)))

{-| Parse 1 or more occurrences of the given character

>>> match (plus anyChar) "123"
["123","12","1"]
>>> match (plus anyChar <* eof) "123"
["123"]
>>> match (plus anyChar) ""
[]
-}
plus :: Pattern Char -> Pattern Text
plus p = fmap Text.pack (some p)

{-| Apply the patterns in the list in order, until one of them succeeds

>>> match (choice [text "cat", text "dog", text "egg"]) "egg"
["egg"]
>>> match (choice [text "cat", text "dog", text "egg"]) "cat"
["cat"]
>>> match (choice [text "cat", text "dog", text "egg"]) "fan"
[]
-}
choice :: [Pattern a] -> Pattern a
choice = msum

{-| Apply the given pattern a fixed number of times, collecting the results

>>> match (count 2 anyChar) "123"
["12"]
>>> match (count 4 anyChar) "123"
[]
-}
count :: Int -> Pattern a -> Pattern [a]
count = replicateM

{-| @between open close p@ matches @p@ in between @open@ and @close@

>>> match (between (char '(') (char ')') (star anyChar)) "(123)"
["123"]
>>> match (between (char '(') (char ')') (star anyChar)) "(123))"
["123)","123"]
>>> match (between (char '(') (char ')') (star anyChar)) "(123"
[]
-}
between :: Pattern a -> Pattern b -> Pattern c -> Pattern c
between open close p = open *> p <* close

{-| Discard the pattern's result

>>> match (skip anyChar) "123"
[()]
>>> match (skip anyChar) ""
[]
-}
skip :: Pattern a -> Pattern ()
skip = void

{-| Restrict the pattern to consume no more than the given number of characters

>>> match (within 2 decimal) "123"
[12,1]
>>> match (within 2 decimal) "1a3"
[1]
-}
within :: Int -> Pattern a -> Pattern a
within n p = Pattern (do
    txt <- get
    let (prefix, suffix) = Text.splitAt n txt
    put prefix
    a <- runPattern p
    modify (<> suffix)
    return a )

{-| Require the pattern to consume exactly the given number of characters

>>> match (fixed 2 decimal) "123"
[12]
>>> match (fixed 2 decimal) "1a3"
[]
-}
fixed :: Int -> Pattern a -> Pattern a
fixed n p = within n (p <* eof)

{-| @p `sepBy` sep@ matches zero or more occurrences of @p@ separated by @sep@

>>> match (decimal `sepBy` char ',') "1,2,3"
[[1,2,3],[1,2],[1],[]]
>>> match (decimal `sepBy` char ',' <* eof) "1,2,3"
[[1,2,3]]
>>> match (decimal `sepBy` char ',') ""
[[]]
-}
sepBy :: Pattern a -> Pattern b -> Pattern [a]
p `sepBy` sep = (p `sepBy1` sep) <|> pure []

{-| @p `sepBy1` sep@ matches one or more occurrences of @p@ separated by @sep@

>>> match (decimal `sepBy1` char ',') "1,2,3"
[[1,2,3],[1,2],[1]]
>>> match (decimal `sepBy1` char ',' <* eof) "1,2,3"
[[1,2,3]]
>>> match (decimal `sepBy1` char ',') ""
[]
-}
sepBy1 :: Pattern a -> Pattern b -> Pattern [a]
p `sepBy1` sep = (:) <$> p <*> many (sep *> p) 
