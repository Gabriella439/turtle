{-# LANGUAGE OverloadedStrings #-}

import Turtle
import System.IO.Temp (withSystemTempDirectory)
import qualified Control.Monad.Fail as Fail
import Control.Monad (unless)

check :: String -> Bool-> IO ()
check errorMessage successs = unless successs $ Fail.fail errorMessage

main :: IO ()
main = withSystemTempDirectory "tempDir" (runTest . fromString)

runTest :: Turtle.FilePath -> IO ()
runTest tempDir = do
  let srcDirectory = tempDir </> "src"

  mktree $ srcDirectory </> "directory"
  touch $ srcDirectory </> "directory" </> "file"

  let destDirectory = tempDir </> "dest"

  cptree srcDirectory destDirectory

  testdir (destDirectory </> "directory") >>= check "cptree did not preserve directory"
  testfile (destDirectory </> "directory" </> "file") >>= check "cptree did not preserve directory"
