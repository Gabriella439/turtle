module Turtle.Util where

import Control.Applicative (Alternative(..))

import Control.Monad (guard)
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
