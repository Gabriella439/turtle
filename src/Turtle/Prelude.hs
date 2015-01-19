{-# LANGUAGE CPP #-}

{-| These are derived utilities built on the primitives exposed by other
    modules
-}

module Turtle.Prelude (
    -- * Shell
      system
    , stream

    -- * Filesystem
    , cd
    , pwd
    , home
    , realpath
    , ls
    , lsTree
    , mv
    , mkdir
    , mktree
    , cp
    , rm
    , rmdir
    , rmtree
    , du
    , testFile
    , testDir

    -- * Utilities
    , cat
    , grep
    , sed
    , yes
    , date

    -- * Input and output
    , stdIn
    , fileIn
    , handleIn
    , stdOut
    , fileOut
    , handleOut
    , fileAppend

    -- * Resources
    , readHandle
    , writeHandle
    , fork
    ) where

import Control.Applicative (Alternative(..))
import Control.Concurrent.Async (Async, async, cancel, wait, withAsync)
import Control.Exception (bracket)
import Control.Foldl (FoldM(..))
import Control.Monad (guard, msum)
#ifdef mingw32_HOST_OS
import Data.Bits ((.&.))
#endif
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Filesystem
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as Filesystem
import System.IO (Handle)
import System.Directory (getPermissions, readable)
import System.Exit (ExitCode)
import qualified System.IO as IO
import qualified System.Process as Process
#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#else
import System.Posix (openDirStream, readDirStream, closeDirStream)
#endif
import Prelude hiding (FilePath)

import Turtle.Pattern (Pattern, anyChar, match, selfless, plus, star)
import Turtle.Protected
import Turtle.Shell

{-| Call a system command, and retrieve the exit code

    The command inherits @stdout@ and @stderr@ for the current process
-}
system
    :: Text
    -- ^ Command
    -> Shell Text
    -- ^ Lines of standard input
    -> IO ExitCode
    -- ^ Exit code
system cmd s = do
    let p = (Process.shell (Text.unpack cmd))
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            }
    (Just hIn, Nothing, Nothing, ph) <- liftIO (Process.createProcess p)
    let feedIn = sh (do
            txt <- s
            liftIO (Text.hPutStrLn hIn txt) )
    withAsync feedIn (\a -> do
        liftIO (wait a)
        liftIO (Process.waitForProcess ph) )

{-| Stream a system command as lines of `Text`

    The command inherits @stderr@ for the current process
-}
stream
    :: Text
    -- ^ Command
    -> Shell Text
    -- ^ Lines of standard input
    -> Shell Text
    -- ^ Lines of standard output
stream cmd s = do
    let p = (Process.shell (Text.unpack cmd))
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.Inherit
            }
    (Just hIn, Just hOut, Nothing, _) <- liftIO (Process.createProcess p)
    let feedIn = sh (do
            txt <- s
            liftIO (Text.hPutStrLn hIn txt) )
    a <- with (fork feedIn)
    handleIn hOut <|> (liftIO (wait a) >> empty)

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

#ifdef mingw32_HOST_OS
fILE_ATTRIBUTE_REPARSE_POINT :: Win32.FileAttributeOrFlag
fILE_ATTRIBUTE_REPARSE_POINT = 1024

reparsePoint :: Win32.FileAttributeOrFlag -> Bool
reparsePoint attr = fILE_ATTRIBUTE_REPARSE_POINT .&. attr /= 0
#endif

{-| List all immediate children of the given directory, excluding @\".\"@ and
    @\"..\"@
-}
ls :: FilePath -> Shell FilePath
ls path = Shell (\(FoldM step begin done) -> do
    x0 <- begin
    let path' = Filesystem.encodeString path
    canRead <- fmap readable (getPermissions path')
#ifdef mingw32_HOST_OS
    reparse <- fmap reparsePoint (Win32.getFileAttributes path')
    if (canRead && not reparse)
        then bracket
            (Win32.findFirstFile (Filesystem.encodeString (path </> "*")))
            (\(h, _) -> Win32.findClose h)
            (\(h, fdat) -> 
                let loop x = do
                        file' <- Win32.getFindDataFileName fdat
                        let file = Filesystem.decodeString file'
                        x' <- if (file' /= "." && file' /= "..")
                            then step x (path </> file)
                            else return x
                        more <- Win32.findNextFile h fdat
                        if more then loop $! x' else done x'
                loop $! x0 )
        else done x0 )
#else
    if canRead
        then bracket (openDirStream path') closeDirStream (\dirp -> do
            let loop x = do
                    file' <- readDirStream dirp
                    case file' of
                        "" -> done x
                        _  -> do
                            let file = Filesystem.decodeString file'
                            x' <- if (file' /= "." && file' /= "..")
                                then step x (path </> file)
                                else return x
                            loop $! x'
            loop $! x0 )
        else done x0 )
#endif

-- | List all recursive descendents of the given directory
lsTree :: FilePath -> Shell FilePath
lsTree path = do
    child <- ls path
    isDir <- liftIO (testDir child)
    if isDir
        then return child <|> lsTree child
        else return child

-- | Move a file or directory
mv :: FilePath -> FilePath -> IO ()
mv = Filesystem.rename

-- | Create a directory
mkdir :: FilePath -> IO ()
mkdir = Filesystem.createDirectory False

-- | Create a directory tree (equivalent to @mkdir -p@)
mktree :: FilePath -> IO ()
mktree = Filesystem.createTree

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

-- | Check if a file exists
testFile :: FilePath -> IO Bool
testFile = Filesystem.isFile

-- | Check if a directory exists
testDir :: FilePath -> IO Bool
testDir = Filesystem.isDirectory

-- | Combine the output of multiple `Shell`s, in order
cat :: [Shell a] -> Shell a
cat = msum

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

-- | A Stream of @\"y\"@s
yes :: Shell Text
yes = Shell (\(FoldM step begin _) -> do
    x0 <- begin
    let loop x = do
            x' <- step x (Text.pack "y")
            loop $! x'
    loop $! x0 )

-- | Get the current time
date :: IO UTCTime
date = getCurrentTime

-- | Read lines of `Text` from standard input
stdIn :: Shell Text
stdIn = handleIn IO.stdin

-- | Read lines of `Text` from a file
fileIn :: FilePath -> Shell Text
fileIn file = do
    handle <- with (readHandle file)
    handleIn handle

-- | Read lines of `Text` from a `Handle`
handleIn :: Handle -> Shell Text
handleIn handle = Shell (\(FoldM step begin done) -> do
    x0 <- begin
    let loop x = do
            eof <- IO.hIsEOF handle
            if eof
                then done x
                else do
                    txt <- Text.hGetLine handle
                    x'  <- step x txt
                    loop $! x'
    loop $! x0 )

-- | Tee lines of `Text` to standard output
stdOut :: Shell Text -> Shell Text
stdOut = handleOut IO.stdout

-- | Tee lines of `Text` to a file
fileOut :: FilePath -> Shell Text -> Shell Text
fileOut file s = do
    handle <- with (writeHandle file)
    handleOut handle s

-- | Tee lines of `Text` to a `Handle`
handleOut :: Handle -> Shell Text -> Shell Text
handleOut handle s = do
    txt <- s
    liftIO (Text.hPutStrLn handle txt)
    return txt

-- | Tee lines of `Text` to append to a file
fileAppend :: FilePath -> Shell Text -> Shell Text
fileAppend file s = do
    handle <- with (appendHandle file)
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

-- | Acquire a `Protected` append-only `Handle` from a `FilePath`
appendHandle :: FilePath -> Protected Handle
appendHandle file = Protect (do
    handle <- Filesystem.openFile file IO.AppendMode
    return (handle, IO.hClose handle) )

-- | Fork a thread, acquiring an `Async` value
fork :: IO a -> Protected (Async a)
fork io = Protect (do
    a <- async io
    return (a, cancel a) )
