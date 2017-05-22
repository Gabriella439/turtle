{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Filesystem.Path.CurrentOS ()
import System.IO.Temp (withSystemTempDirectory)
import Control.Monad (unless)

check :: String -> Bool-> IO ()
check errorMessage successs = unless successs $ error errorMessage

-- This test fails by hanging
main :: IO ()
main = withSystemTempDirectory "tempDir" (runTest . fromString)

runTest :: Turtle.FilePath -> IO ()
runTest tempDir = do
  let srcDirectory = tempDir </> "src"
  mkdir srcDirectory

  mkdir $ srcDirectory </> "directory"
  touch $ srcDirectory </> "directory" </> "file"

  let destDirectory = tempDir </> "dest"

  cptree srcDirectory destDirectory

  testdir (destDirectory </> "directory") >>= check "cptree did not preserve directory"
  testfile (destDirectory </> "directory" </> "file") >>= check "cptree did not preserve directory"
