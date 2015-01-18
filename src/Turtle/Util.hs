module Turtle.Util where

import Control.Applicative (Alternative(..))

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified System.IO as IO
import Prelude hiding (FilePath)

import Turtle.Pattern (Pattern, anyChar, match, selfless, plus, star)
import Turtle.Resource
import Turtle.Shell

-- | Convert a list to `Shell` that emits each element of the list
select :: [a] -> Shell a
select  []    = empty
select (a:as) = return a <|> select as

-- | Combine the output of multiple `Shell`s, in order
cat :: [Shell a] -> Shell a
cat = foldr (<|>) empty

-- | Keep all lines that match the given `Pattern` anywhere within the line
grep :: Pattern a -> Shell Text -> Shell Text
grep pattern shell = do
    txt <- shell
    let pattern' = do
            _ <- star anyChar
            pattern
    guard (not (null (match pattern' txt)))
    return txt

-- | Replace all occurrences of a `Pattern` with its `Text` result
sed :: Pattern Text -> Shell Text -> Shell Text
sed pattern shell = do
    let pattern' = fmap Text.concat (many (pattern <|> selfless (plus anyChar)))
    txt    <- shell
    txt':_ <- return (match pattern' txt)
    return txt'

-- | Parse a structured value from each line of `Text`
form :: Pattern a -> Shell Text -> Shell a
form pattern shell = do
    txt <- shell
    a:_ <- return (match pattern txt)
    return a

-- | Read lines of `Text` from a `Handle`
handleIn :: Handle -> Shell Text
handleIn handle = do
    eof <- liftIO (IO.hIsEOF handle)
    if eof
        then empty
        else do
            txt <- liftIO (Text.hGetLine handle)
            return txt <|> handleIn handle

-- | Read lines of `Text` from standard input
stdIn :: Shell Text
stdIn = handleIn IO.stdin

-- | Read lines of `Text` from a file
fileIn :: FilePath -> Shell Text
fileIn file = do
    handle <- with (readHandle file)
    handleIn handle

-- | Tee lines of `Text` to a `Handle`
handleOut :: Handle -> Shell Text -> Shell Text
handleOut handle shell = do
    txt <- shell
    liftIO (Text.hPutStrLn handle txt)
    return txt

-- | Tee lines of `Text` to standard output
stdOut :: Shell Text -> Shell Text
stdOut = handleOut IO.stdout

-- | Tee lines of `Text` to a file
fileOut :: FilePath -> Shell Text -> Shell Text
fileOut file shell = do
    handle <- with (writeHandle file)
    handleOut handle shell
