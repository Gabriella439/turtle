{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as Text
import Criterion.Main
import Turtle

main :: IO ()
main = defaultMain
  [ bgroup "Pattern"
      [ let cats = Text.replicate 1000 "cat"
            furniture = Text.replicate 5 "   "
        in bgroup "Cat Lady's House"
             [ bench "Basic"
                $ nf (match (many "cat")) cats
             , bench "Letters"
                $ nf (match (many (mconcat ["c", "a", "t"]))) cats
             , bench "Spaces"
                $ nf (match (many "cat" <* spaces)) (cats <> furniture)
             , bench "Prefix"
                $ nf (match (prefix (many "cat"))) (cats <> furniture)
             ]
      ]
  ]
