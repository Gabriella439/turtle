{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as Text
import Test.Tasty.Bench
import Turtle

boundedNaive :: Int -> Int -> Pattern a -> Pattern [a]
boundedNaive m n p = do
    x <- choice (map pure [m..n])
    count x p

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
      , let hearts n = Text.replicate n "heart"
        in bgroup "Love Knows No Bounds"
            [ bench "500-700:650 Naive"
               $ nf (match (boundedNaive 500 700 "heart")) (hearts 650)
            , bench "500-700:650"
               $ nf (match (bounded 500 700 "heart")) (hearts 650)
            , bench "5000-7000:6500 Naive"
               $ nf (match (boundedNaive 5000 7000 "heart")) (hearts 6500)
            , bench "5000-7000:6500"
               $ nf (match (bounded 5000 7000 "heart")) (hearts 6500)
            ]
      ]
  ]
