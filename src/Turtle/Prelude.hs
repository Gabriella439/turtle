{-| These are derived utilities built on the primitives exposed by other
    modules
-}

module Turtle.Prelude (
    -- * Utilities
      shell
    , cat
    , cd
    , pwd
    , home
    , realpath
    , mv
    , mkdir
    , mktree
    , cp
    , rm
    , rmdir
    , rmtree
    , du
    , grep
    , sed
    , form

    -- * Input and output
    , handleIn
    , stdIn
    , fileIn
    , handleOut
    , stdOut
    , fileOut

    -- * Resources
    , readHandle
    , writeHandle
    , fork
    ) where

import Control.Applicative (Alternative(..))
import Control.Concurrent.Async (Async, async, cancel, wait)
import Control.Monad (guard, msum)
import Data.Text (Text)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Filesystem
import Filesystem.Path (FilePath)
import System.IO (Handle)
import qualified System.IO as IO
import qualified System.Process as Process
import Prelude hiding (FilePath)

import Turtle.Pattern (Pattern, anyChar, match, selfless, plus, star)
import Turtle.Protected
import Turtle.Shell

shell :: Text -> Shell Text -> Shell Text
shell cmd s = do
    (pRead, pWrite) <- liftIO Process.createPipe
    let p = (Process.shell (Text.unpack cmd))
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.UseHandle pWrite
            , Process.std_err = Process.UseHandle pWrite
            }
    (Just hIn, Nothing, Nothing, ph) <- liftIO (Process.createProcess p)
    let feedIn = runShell (do
            txt <- s
            liftIO (Text.hPutStrLn hIn txt) )
    a   <- with (fork feedIn)
    handleIn pRead <|> (liftIO (wait a) >> empty)

-- | Combine the output of multiple `Shell`s, in order
cat :: [Shell a] -> Shell a
cat = msum

-- | Change the current directory
cd :: FilePath -> IO ()
cd = Filesystem.setWorkingDirectory

-- | Get the current directory
pwd :: IO FilePath
pwd = Filesystem.getWorkingDirectory

-- | Get the home directory
home :: IO FilePath
home = Filesystem.getWorkingDirectory

-- | Canonicalize a path
realpath :: FilePath -> IO FilePath
realpath = Filesystem.canonicalizePath

-- | Move a file or directory
mv :: FilePath -> FilePath -> IO ()
mv = Filesystem.rename

-- | Create a directory
mkdir :: FilePath -> IO ()
mkdir = Filesystem.createDirectory False

-- | Create a directory tree (equivalent to @mkdir -p@)
mktree :: FilePath -> IO ()
mktree = Filesystem.createTree

--  List the immediate children of a directory, excluding @\".\"@ and @\"..\"@
-- ls :: FilePath -> Shell String

-- | Copy a file
cp :: FilePath -> FilePath -> IO ()
cp = Filesystem.copyFile

-- | Remove a file
rm :: FilePath -> IO ()
rm = Filesystem.removeFile

-- | Remove a directory
rmdir :: FilePath -> IO ()
rmdir = Filesystem.removeFile

-- | Remove a directory tree
rmtree :: FilePath -> IO ()
rmtree = Filesystem.removeTree

-- | Get a file or directory's size
du :: FilePath -> IO Integer
du = Filesystem.getSize


-- | Keep all lines that match the given `Pattern` anywhere within the line
grep :: Pattern a -> Shell Text -> Shell Text
grep pattern s = do
    txt <- s
    let pattern' = do
            _ <- star anyChar
            pattern
    guard (not (null (match pattern' txt)))
    return txt

-- | Replace all occurrences of a `Pattern` with its `Text` result
sed :: Pattern Text -> Shell Text -> Shell Text
sed pattern s = do
    let pattern' = fmap Text.concat (many (pattern <|> selfless (plus anyChar)))
    txt    <- s
    txt':_ <- return (match pattern' txt)
    return txt'

-- | Parse a structured value from each line of `Text`
form :: Pattern a -> Shell Text -> Shell a
form pattern s = do
    txt <- s
    a:_ <- return (match pattern txt)
    return a

-- | Read lines of `Text` from a `Handle`
handleIn :: Handle -> Shell Text
handleIn handle = do
    eof <- liftIO (IO.hIsEOF handle)
    if eof
        then empty
        else do
            txt <- liftIO (Text.hGetLine handle)
            return txt <|> handleIn handle

-- | Read lines of `Text` from standard input
stdIn :: Shell Text
stdIn = handleIn IO.stdin

-- | Read lines of `Text` from a file
fileIn :: FilePath -> Shell Text
fileIn file = do
    handle <- with (readHandle file)
    handleIn handle

-- | Tee lines of `Text` to a `Handle`
handleOut :: Handle -> Shell Text -> Shell Text
handleOut handle s = do
    txt <- s
    liftIO (Text.hPutStrLn handle txt)
    return txt

-- | Tee lines of `Text` to standard output
stdOut :: Shell Text -> Shell Text
stdOut = handleOut IO.stdout

-- | Tee lines of `Text` to a file
fileOut :: FilePath -> Shell Text -> Shell Text
fileOut file s = do
    handle <- with (writeHandle file)
    handleOut handle s

-- | Acquire a `Protected` read-only `Handle` from a `FilePath`
readHandle :: FilePath -> Protected Handle
readHandle file = Protect (do
    handle <- Filesystem.openFile file IO.ReadMode
    return (handle, IO.hClose handle) )

-- | Acquire a `Protected` write-only `Handle` from a `FilePath`
writeHandle :: FilePath -> Protected Handle
writeHandle file = Protect (do
    handle <- Filesystem.openFile file IO.WriteMode
    return (handle, IO.hClose handle) )

-- | Fork a thread, acquiring an `Async` value
fork :: IO a -> Protected (Async a)
fork io = Protect (do
    a <- async io
    return (a, cancel a) )
