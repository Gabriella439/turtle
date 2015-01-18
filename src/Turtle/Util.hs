module Turtle.Util where

import Control.Applicative (Alternative(..))

import Control.Exception (bracket)
import Control.Monad (guard)
import qualified Data.Text.IO as Text
import System.IO

import Turtle.Parser
import Turtle.Shell

select :: [a] -> Shell a
select  []    = empty
select (a:as) = return a <|> select as

cat :: [Shell a] -> Shell a
cat = foldr (<|>) empty

grep :: Parser a -> Shell Text -> Shell Text
grep p s = do
    str <- s
    guard (not (null (parse p str)))
    return str

fileRead :: FilePath -> Shell Text
fileRead file = Shell (\step begin done -> do
    x0 <- begin
    x1 <- bracket (openFile file ReadMode) hClose (\handle -> do
        let go x = do
                eof <- hIsEOF handle
                if eof
                    then return x
                    else do
                        str <- Text.hGetLine handle
                        x'  <- step x str
                        go x'
        go x0 )
    done x1 )

fileWrite :: FilePath -> FoldM IO Text ()
fileWrite file = FoldM step (openFile file WriteMode) hClose
  where
    step handle txt = do
        Text.hPutStrLn handle txt
        return handle
