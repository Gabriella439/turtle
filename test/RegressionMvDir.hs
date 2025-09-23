{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle

main :: IO ()
main = sh $ do
  baseDir <- using $ mktempdir "." "mv-dir-test"
  mkdir $ baseDir </> "foo"
  mv (baseDir </> "foo") (baseDir </> "bar")
  rmtree baseDir
