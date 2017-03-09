{-# LANGUAGE OverloadedStrings #-}

import Turtle

import qualified System.Timeout

main :: IO ()
main = do
    m <- System.Timeout.timeout 1 (runManaged (do
        _ <- fork (shells "while true; do sleep 1; done" empty)
        return () ))
    case m of
        Nothing -> die "Subprocess runners are incorrectly masking exceptions"
        Just _  -> return ()
