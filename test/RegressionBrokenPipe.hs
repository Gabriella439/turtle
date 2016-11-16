{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import System.Timeout
import Turtle
import qualified Turtle.Bytes as Bytes

main :: IO ()
main = do
    echo "proc (text)"
    void $ timeout duration $ forever $ proc "true" [] message
    echo "procStrict (text)"
    void $ timeout duration $ forever $ procStrict "true" [] message
    echo "procStrictWithErr (text)"
    void $ timeout duration $ forever $ procStrictWithErr "true" [] message
    echo "proc (bytes)"
    void $ timeout duration $ forever $ Bytes.proc "true" [] message
    echo "procStrict (bytes)"
    void $ timeout duration $ forever $ Bytes.procStrict "true" [] message
    echo "procStrictWithErr (bytes)"
    void $ timeout duration $ forever $ Bytes.procStrictWithErr "true" [] message
  where
    message :: IsString s => s
    message = "foobarbaz"
    duration = 3 * 10^ (6 :: Int)
