module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Turtle/Pattern.hs", "src/Turtle/Format.hs"]
