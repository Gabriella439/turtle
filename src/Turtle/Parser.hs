{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

{-| The simplest possible parser implementation

    Example usage:

>>> :set -XOverloadedStrings
>>> parse (text "cat" <|> text "dog") "cat"
["cat"]
>>> parse (some (notChar ',') <* char ',') "cat,dog"
["cat"]
>>> parse (count 3 anyChar) "cat,dog"
["cat"]

    This parser has unlimited backtracking, and will return as many solutions as
    possible:

>>> parse (some anyChar) "123"
["123","12","1"]
>>> parse (some anyChar <* eof) "123"
["123"]

    Use @do@ notation to structure more complex parsers:

>>> :{
let bit = (char '0' *> pure False) <|> (char '1' *> pure True);
    portableBitMap = do
        { text "P1"
        ; width  <- spaces1 *> decimal
        ; height <- spaces1 *> decimal
        ; count width (count height (spaces1 *> bit))
        };
in  parse portableBitMap "P1\n2 2\n0 0\n1 0\n"
:}
[[[False,False],[True,False]]]

-}

module Turtle.Parser (
    -- * Parser
      Parser
    , parse

    -- * Primitive parsers
    , anyChar
    , eof

    -- * Character parsers
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

newtype Parser a = Parser { runParser :: StateT Text [] a }
    deriving (Functor, Applicative, Monad, Alternative, MonadPlus)

instance Monoid a => Monoid (Parser a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance Num a => Num (Parser a) where
    fromInteger n = pure (fromInteger n)

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

    abs    = fmap abs
    signum = fmap signum
    negate = fmap negate

instance Fractional a => Fractional (Parser a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance Floating a => Floating (Parser a) where
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

instance (a ~ Text) => IsString (Parser a) where
    fromString str = text (Text.pack str)

-- | Supply a `Parser` with a `Text` input, returning all possible solutions
parse :: Parser a -> Text -> [a]
parse p = evalStateT (runParser p)

{-| Match any character

>>> parse anyChar "123"
"1"
>>> parse anyChar ""
""

-}
anyChar :: Parser Char
anyChar = Parser (do
    Just (c, cs) <- fmap Text.uncons get
    put cs
    return c )

{-| Matches the end of input

>>> parse eof "123"
[]
>>> parse eof ""
[()]

-}
eof :: Parser ()
eof = Parser (do
    True <- fmap Text.null get
    return () )

{-| Match any character that satisfies the given predicate

>>> parse (satisfy (== '1')) "123"
"1"
>>> parse (satisfy (== '2')) "123"
""

-}
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = do
    c <- anyChar
    guard (predicate c)
    return c

{-| Match a specific character

>>> parse (char '1') "123"
"1"
>>> parse (char '2') "123"
""

-}
char :: Char -> Parser Char
char c = satisfy (== c)

{-| Match any character except the given one

>>> parse (notChar '2') "123"
"1"
>>> parse (notChar '1') "123"
""

-}
notChar :: Char -> Parser Char
notChar c = satisfy (/= c)

{-| Match a specific string

>>> parse (text "12") "123"
["12"]

-}
text :: Text -> Parser Text
text prefix' = Parser (do
    txt <- get
    let (prefix, suffix) = Text.splitAt (Text.length prefix') txt
    guard (prefix == prefix')
    put suffix
    return prefix )

{-| Match any one of the given characters

>>> parse (oneOf "1a") "123"
"1"
>>> parse (oneOf "2a") "123"
""

-}
oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

{-| Match anything other than the given characters

>>> parse (noneOf "2a") "123"
"1"
>>> parse (noneOf "1a") "123"
""

-}
noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (`notElem` cs)

{-| Match a whitespace character

>>> parse space " a"
" "
>>> parse space "a "
""

-}
space :: Parser Char
space = satisfy isSpace

{-| Match zero or more whitespace characters

>>> parse spaces "  "
["  "," ",""]
>>> parse spaces "a "
[""]

-}
spaces :: Parser Text
spaces = star space

{-| Match one or more whitespace characters

>>> parse spaces1 "  "
["  "," "]
>>> parse spaces1 "a "
[]

-}
spaces1 :: Parser Text
spaces1 = plus space

{-| Match the tab character (@\'\t\'@)

>>> parse tab "\t"
"\t"
>>> parse tab " "
""
-}
tab :: Parser Char
tab = char '\t'

{-| Match the newline character (@\'\n\'@)

>>> parse newline "\n"
"\n"
>>> parse newline " "
""
-}
newline :: Parser Char
newline = char '\n'

{-| Matches a carriage return (@\'\r\'@) followed by a newline (@\'\n\'@)

>>> parse crlf "\r\n"
["\r\n"]
>>> parse crlf "\n\r"
[]
-}
crlf :: Parser Text
crlf = text "\r\n"

{-| Match an uppercase letter

>>> parse upper "ABC"
"A"
>>> parse upper "abc"
""
-}
upper :: Parser Char
upper = satisfy isUpper

{-| Match a lowercase letter

>>> parse lower "abc"
"a"
>>> parse lower "ABC"
""
-}
lower :: Parser Char
lower = satisfy isLower

{-| Match a letter or digit

>>> parse alphaNum "123"
"1"
>>> parse alphaNum "abc"
"a"
>>> parse alphaNum "ABC"
"A"
>>> parse alphaNum "..."
""
-}
alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

{-| Match a letter

>>> parse letter "ABC"
"A"
>>> parse letter "abc"
"a"
>>> parse letter "123"
""
-}
letter :: Parser Char
letter = satisfy isLetter

{-| Match a digit

>>> parse digit "123"
"1"
>>> parse digit "abc"
""
-}
digit :: Parser Char
digit = satisfy isDigit

{-| Match a hexadecimal digit

>>> parse hexDigit "123"
"1"
>>> parse hexDigit "ABC"
"A"
>>> parse hexDigit "abc"
"a"
>>> parse hexDigit "ghi"
""
-}
hexDigit :: Parser Char
hexDigit = satisfy isHexDigit

{-| Match an octal digit

>>> parse octDigit "123"
"1"
>>> parse octDigit "9"
""
-}
octDigit :: Parser Char
octDigit = satisfy isOctDigit

{-| Match an unsigned decimal number

>>> parse (decimal <* eof) "123"
[123]
>>> parse (decimal <* eof) "-123"
[]
-}
decimal :: Num n => Parser n
decimal = do
    ds <- some digit
    return (foldl' step 0 ds)
  where
    step n d = n * 10 + fromIntegral (ord d - ord '0')

{-| Parse a number with an optional leading @\'+\'@ or @\'-\'@ sign

>>> parse (signed decimal <* eof) "+123"
[123]
>>> parse (signed decimal <* eof) "-123"
[-123]
>>> parse (signed decimal <* eof)  "123"
[123]
-}
signed :: Num a => Parser a -> Parser a
signed p = do
    sign <- (char '+' *> pure id) <|> (char '-' *> pure negate) <|> (pure id)
    fmap sign p

{-| Parser 0 or more occurrecnes of the given character

>>> parse (star anyChar) "123"
["123","12","1",""]
>>> parse (star anyChar <* eof) "123"
["123"]
>>> parse (star anyChar) ""
[""]
-}
star :: Parser Char -> Parser Text
star p = fmap Text.pack (many p)

{-| Parse 1 or more occurrences of the given character

>>> parse (some anyChar) "123"
["123","12","1"]
>>> parse (some anyChar <* eof) "123"
["123"]
>>> parse (some anyChar) ""
[]
-}
plus :: Parser Char -> Parser Text
plus p = fmap Text.pack (some p)

{-| Apply the parsers in the list in order, until one of them succeeds

>>> parse (choice [text "cat", text "dog", text "egg"]) "egg"
["egg"]
>>> parse (choice [text "cat", text "dog", text "egg"]) "cat"
["cat"]
>>> parse (choice [text "cat", text "dog", text "egg"]) "fan"
[]
-}
choice :: [Parser a] -> Parser a
choice = msum

{-| Apply the given parser a fixed number of times, collecting the results

>>> parse (count 2 anyChar) "123"
["12"]
>>> parse (count 4 anyChar) "123"
[]
-}
count :: Int -> Parser a -> Parser [a]
count = replicateM

{-| @between open close p@ parses @p@ in between @open@ and @close@

>>> parse (between (char '(') (char ')') (star anyChar)) "(123)"
["123"]
>>> parse (between (char '(') (char ')') (star anyChar)) "(123))"
["123)","123"]
>>> parse (between (char '(') (char ')') (star anyChar)) "(123"
[]
-}
between :: Parser a -> Parser b -> Parser c -> Parser c
between open close p = open *> p <* close

{-| Discard the parser's result

>>> parse (skip anyChar) "123"
[()]
>>> parse (skip anyChar) ""
[]
-}
skip :: Parser a -> Parser ()
skip = void

{-| Restrict the parser to consume no more than the given number of characters

>>> parse (within 2 decimal) "123"
[12,1]
>>> parse (within 2 decimal) "1a3"
[1]
-}
within :: Int -> Parser a -> Parser a
within n p = Parser (do
    txt <- get
    let (prefix, suffix) = Text.splitAt n txt
    put prefix
    a <- runParser p
    modify (<> suffix)
    return a )

{-| Require the parser to consume exactly the given number of characters

>>> parse (fixed 2 decimal) "123"
[12]
>>> parse (fixed 2 decimal) "1a3"
[]
-}
fixed :: Int -> Parser a -> Parser a
fixed n p = within n (p <* eof)

{-| @p `sepBy` sep@ parses zero or more occurrences of @p@ separated by @sep@

>>> parse (decimal `sepBy` char ',') "1,2,3"
[[1,2,3],[1,2],[1],[]]
>>> parse (decimal `sepBy` char ',' <* eof) "1,2,3"
[[1,2,3]]
>>> parse (decimal `sepBy` char ',') ""
[[]]
-}
sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = (p `sepBy1` sep) <|> pure []

{-| @p `sepBy1` sep@ parses one or more occurrences of @p@ separated by @sep@

>>> parse (decimal `sepBy1` char ',') "1,2,3"
[[1,2,3],[1,2],[1]]
>>> parse (decimal `sepBy1` char ',' <* eof) "1,2,3"
[[1,2,3]]
>>> parse (decimal `sepBy1` char ',') ""
[]
-}
sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = (:) <$> p <*> many (sep *> p) 
