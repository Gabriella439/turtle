{-# LANGUAGE OverloadedStrings #-}

import Turtle

-- This test fails by hanging
main :: IO ()
main = runManaged (do
    _ <- fork (shells "while true; do sleep 1; done" empty)
    sleep 1
    return () )
