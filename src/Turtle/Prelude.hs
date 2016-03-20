{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

-- | This module provides a large suite of utilities that resemble Unix
--  utilities.
--
--  Many of these commands are just existing Haskell commands renamed to match
--  their Unix counterparts:
--
-- >>> :set -XOverloadedStrings
-- >>> cd "/tmp"
-- >>> pwd
-- FilePath "/tmp"
--
-- Some commands are `Shell`s that emit streams of values.  `view` prints all
-- values in a `Shell` stream:
--
-- >>> view (ls "/usr")
-- FilePath "/usr/lib"
-- FilePath "/usr/src"
-- FilePath "/usr/sbin"
-- FilePath "/usr/include"
-- FilePath "/usr/share"
-- FilePath "/usr/games"
-- FilePath "/usr/local"
-- FilePath "/usr/bin"
-- >>> view (find (suffix "Browser.py") "/usr/lib")
-- FilePath "/usr/lib/python3.4/idlelib/ClassBrowser.py"
-- FilePath "/usr/lib/python3.4/idlelib/RemoteObjectBrowser.py"
-- FilePath "/usr/lib/python3.4/idlelib/PathBrowser.py"
-- FilePath "/usr/lib/python3.4/idlelib/ObjectBrowser.py"
--
-- Use `fold` to reduce the output of a `Shell` stream:
--
-- >>> import qualified Control.Foldl as Fold
-- >>> fold (ls "/usr") Fold.length
-- 8
-- >>> fold (find (suffix "Browser.py") "/usr/lib") Fold.head
-- Just (FilePath "/usr/lib/python3.4/idlelib/ClassBrowser.py")
--
-- Create files using `output`:
--
-- >>> output "foo.txt" ("123" <|> "456" <|> "ABC")
-- >>> realpath "foo.txt"
-- FilePath "/tmp/foo.txt"
--
-- Read in files using `input`:
--
-- >>> stdout (input "foo.txt")
-- 123
-- 456
-- ABC
--
-- Format strings in a type safe way using `format`:
--
-- >>> dir <- pwd
-- >>> format ("I am in the "%fp%" directory") dir
-- "I am in the /tmp directory"
--
-- Commands like `grep`, `sed` and `find` accept arbitrary `Pattern`s
--
-- >>> stdout (grep ("123" <|> "ABC") (input "foo.txt"))
-- 123
-- ABC
-- >>> let exclaim = fmap (<> "!") (plus digit)
-- >>> stdout (sed exclaim (input "foo.txt"))
-- 123!
-- 456!
-- ABC
--
-- Note that `grep` and `find` differ from their Unix counterparts by requiring
-- that the `Pattern` matches the entire line or file name by default.  However,
-- you can optionally match the prefix, suffix, or interior of a line:
--
-- >>> stdout (grep (has    "2") (input "foo.txt"))
-- 123
-- >>> stdout (grep (prefix "1") (input "foo.txt"))
-- 123
-- >>> stdout (grep (suffix "3") (input "foo.txt"))
-- 123
--
--  You can also build up more sophisticated `Shell` programs using `sh` in
--  conjunction with @do@ notation:
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >
-- >import Turtle
-- >
-- >main = sh example
-- >
-- >example = do
-- >    -- Read in file names from "files1.txt" and "files2.txt"
-- >    file <- fmap fromText (input "files1.txt" <|> input "files2.txt")
-- >
-- >    -- Stream each file to standard output only if the file exists
-- >    True <- liftIO (testfile file)
-- >    line <- input file
-- >    liftIO (echo line)
--
-- See "Turtle.Tutorial" for an extended tutorial explaining how to use this
-- library in greater detail.

module Turtle.Prelude (
    -- * IO
      echo
    , err
    , readline
    , Filesystem.readTextFile
    , Filesystem.writeTextFile
    , arguments
#if MIN_VERSION_base(4,7,0)
    , export
    , unset
#endif
#if MIN_VERSION_base(4,6,0)
    , need
#endif
    , env
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
    , testfile
    , testdir
    , testpath
    , date
    , datefile
    , touch
    , time
    , hostname
    , sleep
    , exit
    , die
    , (.&&.)
    , (.||.)

    -- * Managed
    , readonly
    , writeonly
    , appendonly
    , mktemp
    , mktempfile
    , mktempdir
    , fork
    , wait

    -- * Shell
    , inproc
    , inshell
    , inprocWithErr
    , inshellWithErr
    , stdin
    , input
    , inhandle
    , stdout
    , output
    , outhandle
    , append
    , stderr
    , strict
    , ls
    , lsif
    , lstree
    , cat
    , grep
    , sed
    , inplace
    , find
    , yes
    , nl
    , paste
    , endless
    , limit
    , limitWhile
    , cache

    -- * Folds
    , countChars
    , countWords
    , countLines

    -- * Text
    , cut

    -- * Subprocess management
    , proc
    , shell
    , system
    , procs
    , shells
    , procStrict
    , shellStrict

    -- * Permissions
    , Permissions
    , chmod
    , getmod
    , setmod
    , copymod
    , readable, nonreadable
    , writable, nonwritable
    , executable, nonexecutable
    , searchable, nonsearchable
    , ooo,roo,owo,oox,oos,rwo,rox,ros,owx,rwx,rws

    -- * File size
    , du
    , Size
    , sz
    , bytes
    , kilobytes
    , megabytes
    , gigabytes
    , terabytes
    , kibibytes
    , mebibytes
    , gibibytes
    , tebibytes

    -- * Exceptions
    , ProcFailed(..)
    , ShellFailed(..)
    ) where

import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
    (Async, withAsync, withAsyncWithUnmask, wait, waitSTM, concurrently)
import Control.Concurrent.MVar (newMVar, modifyMVar_)
import qualified Control.Concurrent.STM as STM
import qualified Control.Concurrent.STM.TQueue as TQueue
import Control.Exception (Exception, bracket, finally, mask_, throwIO)
import Control.Foldl (Fold, FoldM(..), genericLength, handles, list, premap)
import qualified Control.Foldl.Text
import Control.Monad (liftM, msum, when, unless)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Managed (Managed, managed, runManaged)
#ifdef mingw32_HOST_OS
import Data.Bits ((.&.))
#endif
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text, pack, unpack)
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Traversable
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import Data.Typeable (Typeable)
import qualified Filesystem
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as Filesystem
import GHC.IO.Exception (IOErrorType(UnsupportedOperation))
import Network.HostName (getHostName)
import System.Clock (Clock(..), TimeSpec(..), getTime)
import System.Environment (
    getArgs,
#if MIN_VERSION_base(4,7,0)
    setEnv,
    unsetEnv,
#endif
#if MIN_VERSION_base(4,6,0)
    lookupEnv,
#endif
    getEnvironment )
import System.Directory (Permissions)
import qualified System.Directory as Directory
import System.Exit (ExitCode(..), exitWith)
import System.IO (Handle, hClose)
import qualified System.IO as IO
import System.IO.Temp (withTempDirectory, withTempFile)
import System.IO.Error (catchIOError, ioeGetErrorType)
import qualified System.Process as Process
#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#else
import System.Posix (
    openDirStream,
    readDirStream,
    closeDirStream,
    touchFile,
    getSymbolicLinkStatus,
    isDirectory,
    isSymbolicLink )
#endif
import Prelude hiding (FilePath)

import Turtle.Pattern (Pattern, anyChar, chars, match, selfless, sepBy)
import Turtle.Shell
import Turtle.Format (Format, format, makeFormat, d, w, (%))

{-| Run a command using @execvp@, retrieving the exit code

    The command inherits @stdout@ and @stderr@ for the current process
-}
proc
    :: MonadIO io
    => Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell Text
    -- ^ Lines of standard input
    -> io ExitCode
    -- ^ Exit code
proc cmd args =
    system
        ( (Process.proc (unpack cmd) (map unpack args))
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            } )

{-| Run a command line using the shell, retrieving the exit code

    This command is more powerful than `proc`, but highly vulnerable to code
    injection if you template the command line with untrusted input

    The command inherits @stdout@ and @stderr@ for the current process
-}
shell
    :: MonadIO io
    => Text
    -- ^ Command line
    -> Shell Text
    -- ^ Lines of standard input
    -> io ExitCode
    -- ^ Exit code
shell cmdLine =
    system
        ( (Process.shell (unpack cmdLine))
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            } )

data ProcFailed = ProcFailed
    { procCommand   :: Text
    , procArguments :: [Text]
    , procExitCode  :: ExitCode
    } deriving (Show, Typeable)

instance Exception ProcFailed

{-| This function is identical to `proc` except this throws `ProcFailed` for
    non-zero exit codes
-}
procs
    :: MonadIO io
    => Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell Text
    -- ^ Lines of standard input
    -> io ()
procs cmd args s = do
    exitCode <- proc cmd args s
    case exitCode of
        ExitSuccess -> return ()
        _           -> liftIO (throwIO (ProcFailed cmd args exitCode))

data ShellFailed = ShellFailed
    { shellCommandLine :: Text
    , shellExitCode    :: ExitCode
    } deriving (Show, Typeable)

instance Exception ShellFailed

{-| This function is identical to `shell` except this throws `ShellFailed` for
    non-zero exit codes
-}
shells
    :: MonadIO io
    => Text
    -- ^ Command line
    -> Shell Text
    -- ^ Lines of standard input
    -> io ()
    -- ^ Exit code
shells cmdline s = do
    exitCode <- shell cmdline s
    case exitCode of
        ExitSuccess -> return ()
        _           -> liftIO (throwIO (ShellFailed cmdline exitCode))

{-| Run a command using @execvp@, retrieving the exit code and stdout as a
    non-lazy blob of Text

    The command inherits @stderr@ for the current process
-}
procStrict
    :: MonadIO io
    => Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell Text
    -- ^ Lines of standard input
    -> io (ExitCode, Text)
    -- ^ Exit code and stdout
procStrict cmd args =
    systemStrict (Process.proc (Text.unpack cmd) (map Text.unpack args))

{-| Run a command line using the shell, retrieving the exit code and stdout as a
    non-lazy blob of Text

    This command is more powerful than `proc`, but highly vulnerable to code
    injection if you template the command line with untrusted input

    The command inherits @stderr@ for the current process
-}
shellStrict
    :: MonadIO io
    => Text
    -- ^ Command line
    -> Shell Text
    -- ^ Lines of standard input
    -> io (ExitCode, Text)
    -- ^ Exit code and stdout
shellStrict cmdLine = systemStrict (Process.shell (Text.unpack cmdLine))

{-| `system` generalizes `shell` and `proc` by allowing you to supply your own
    custom `CreateProcess`.  This is for advanced users who feel comfortable
    using the lower-level @process@ API
-}
system
    :: MonadIO io
    => Process.CreateProcess
    -- ^ Command
    -> Shell Text
    -- ^ Lines of standard input
    -> io ExitCode
    -- ^ Exit code
system p s = liftIO (do
    let open = do
            (Just hIn, Nothing, Nothing, ph) <- Process.createProcess p
            IO.hSetBuffering hIn IO.LineBuffering
            return (hIn, ph)

    -- Prevent double close
    mvar <- newMVar False
    let close handle = do
            modifyMVar_ mvar (\finalized -> do
                unless finalized (hClose handle)
                return True )

    bracket open (\(hIn, ph) -> close hIn >> Process.terminateProcess ph) (\(hIn, ph) -> do
        let feedIn :: (forall a. IO a -> IO a) -> IO ()
            feedIn restore =
                restore (sh (do
                    txt <- s
                    liftIO (Text.hPutStrLn hIn txt) ) )
                `finally` close hIn
        mask_ (withAsyncWithUnmask feedIn (\a -> liftIO (Process.waitForProcess ph) <* wait a) ) ) )

systemStrict
    :: MonadIO io
    => Process.CreateProcess
    -- ^ Command
    -> Shell Text
    -- ^ Lines of standard input
    -> io (ExitCode, Text)
    -- ^ Exit code and stdout
systemStrict p s = liftIO (do
    let p' = p
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.Inherit
            }

    let open = do
            (Just hIn, Just hOut, Nothing, ph) <- liftIO (Process.createProcess p')
            IO.hSetBuffering hIn IO.LineBuffering
            return (hIn, hOut, ph)

    -- Prevent double close
    mvar <- newMVar False
    let close handle = do
            modifyMVar_ mvar (\finalized -> do
                unless finalized (hClose handle)
                return True )

    bracket open (\(hIn, _, ph) -> close hIn >> Process.terminateProcess ph) (\(hIn, hOut, ph) -> do
        let feedIn :: (forall a. IO a -> IO a) -> IO ()
            feedIn restore =
                restore (sh (do
                    txt <- s
                    liftIO (Text.hPutStrLn hIn txt) ) )
                `finally` close hIn

        concurrently
            (mask_ (withAsyncWithUnmask feedIn (\a -> liftIO (Process.waitForProcess ph) <* wait a)))
            (Text.hGetContents hOut) ) )

{-| Run a command using @execvp@, streaming @stdout@ as lines of `Text`

    The command inherits @stderr@ for the current process
-}
inproc
    :: Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell Text
    -- ^ Lines of standard input
    -> Shell Text
    -- ^ Lines of standard output
inproc cmd args = stream (Process.proc (unpack cmd) (map unpack args))

{-| Run a command line using the shell, streaming @stdout@ as lines of `Text`

    This command is more powerful than `inproc`, but highly vulnerable to code
    injection if you template the command line with untrusted input

    The command inherits @stderr@ for the current process
-}
inshell
    :: Text
    -- ^ Command line
    -> Shell Text
    -- ^ Lines of standard input
    -> Shell Text
    -- ^ Lines of standard output
inshell cmd = stream (Process.shell (unpack cmd))

stream
    :: Process.CreateProcess
    -- ^ Command
    -> Shell Text
    -- ^ Lines of standard input
    -> Shell Text
    -- ^ Lines of standard output
stream p s = do
    let p' = p
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.Inherit
            }

    let open = do
            (Just hIn, Just hOut, Nothing, ph) <- liftIO (Process.createProcess p')
            IO.hSetBuffering hIn IO.LineBuffering
            return (hIn, hOut, ph)

    -- Prevent double close
    mvar <- liftIO (newMVar False)
    let close handle = do
            modifyMVar_ mvar (\finalized -> do
                unless finalized (hClose handle)
                return True )

    (hIn, hOut, ph) <- using (managed (bracket open (\(hIn, _, ph) -> close hIn >> Process.terminateProcess ph)))
    let feedIn :: (forall a. IO a -> IO a) -> IO ()
        feedIn restore =
            restore (sh (do
                txt <- s
                liftIO (Text.hPutStrLn hIn txt) ) )
            `finally` close hIn

    a <- using (managed (mask_ . withAsyncWithUnmask feedIn))
    inhandle hOut <|> (liftIO (Process.waitForProcess ph *> wait a) *> empty)

streamWithErr
    :: Process.CreateProcess
    -- ^ Command
    -> Shell Text
    -- ^ Lines of standard input
    -> Shell (Either Text Text)
    -- ^ Lines of standard output
streamWithErr p s = do
    let p' = p
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }

    let open = do
            (Just hIn, Just hOut, Just hErr, ph) <- liftIO (Process.createProcess p')
            IO.hSetBuffering hIn IO.LineBuffering
            return (hIn, hOut, hErr, ph)

    -- Prevent double close
    mvar <- liftIO (newMVar False)
    let close handle = do
            modifyMVar_ mvar (\finalized -> do
                unless finalized (hClose handle)
                return True )

    (hIn, hOut, hErr, ph) <- using (managed (bracket open (\(hIn, _, _, ph) -> close hIn >> Process.terminateProcess ph)))
    let feedIn :: (forall a. IO a -> IO a) -> IO ()
        feedIn restore =
            restore (sh (do
                txt <- s
                liftIO (Text.hPutStrLn hIn txt) ) )
            `finally` close hIn

    queue <- liftIO TQueue.newTQueueIO
    let forwardOut :: (forall a. IO a -> IO a) -> IO ()
        forwardOut restore =
            restore (sh (do
                txt <- inhandle hOut
                liftIO (STM.atomically (TQueue.writeTQueue queue (Just (Right txt)))) ))
            `finally` STM.atomically (TQueue.writeTQueue queue Nothing)
    let forwardErr :: (forall a. IO a -> IO a) -> IO ()
        forwardErr restore =
            restore (sh (do
                txt <- inhandle hErr
                liftIO (STM.atomically (TQueue.writeTQueue queue (Just (Left  txt)))) ))
            `finally` STM.atomically (TQueue.writeTQueue queue Nothing)
    let drain = Shell (\(FoldM step begin done) -> do
            x0 <- begin
            let loop x numNothing
                    | numNothing < 2 = do
                        m <- STM.atomically (TQueue.readTQueue queue)
                        case m of
                            Nothing -> loop x $! numNothing + 1
                            Just e  -> do
                                x' <- step x e
                                loop x' numNothing
                    | otherwise      = return x
            x1 <- loop x0 (0 :: Int)
            done x1 )

    a <- using (managed (mask_ . withAsyncWithUnmask feedIn    ))
    b <- using (managed (mask_ . withAsyncWithUnmask forwardOut))
    c <- using (managed (mask_ . withAsyncWithUnmask forwardErr))
    let l `also` r = do
            _ <- l <|> (r *> STM.retry)
            _ <- r
            return ()
    let waitAll = STM.atomically (waitSTM a `also` (waitSTM b `also` waitSTM c))
    drain <|> (liftIO (Process.waitForProcess ph *> waitAll) *> empty)

{-| Run a command using the shell, streaming @stdout@ and @stderr@ as lines of
    `Text`.  Lines from @stdout@ are wrapped in `Right` and lines from @stderr@
    are wrapped in `Left`.  This does /not/ throw an exception if the command
    returns a non-zero exit code
-}
inprocWithErr
    :: Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell Text
    -- ^ Lines of standard input
    -> Shell (Either Text Text)
    -- ^ Lines of either standard output (`Right`) or standard error (`Left`)
inprocWithErr cmd args =
    streamWithErr (Process.proc (unpack cmd) (map unpack args))


{-| Run a command line using the shell, streaming @stdout@ and @stderr@ as lines
    of `Text`.  Lines from @stdout@ are wrapped in `Right` and lines from
    @stderr@ are wrapped in `Left`.  This does /not/ throw an exception if the
    command returns a non-zero exit code

    This command is more powerful than `inprocWithErr`, but highly vulnerable to
    code injection if you template the command line with untrusted input
-}
inshellWithErr
    :: Text
    -- ^ Command line
    -> Shell Text
    -- ^ Lines of standard input
    -> Shell (Either Text Text)
    -- ^ Lines of either standard output (`Right`) or standard error (`Left`)
inshellWithErr cmd = streamWithErr (Process.shell (unpack cmd))

-- | Print to @stdout@
echo :: MonadIO io => Text -> io ()
echo txt = liftIO (Text.putStrLn txt)

-- | Print to @stderr@
err :: MonadIO io => Text -> io ()
err txt = liftIO (Text.hPutStrLn IO.stderr txt)

{-| Read in a line from @stdin@

    Returns `Nothing` if at end of input
-}
readline :: MonadIO io => io (Maybe Text)
readline = liftIO (do
    eof <- IO.isEOF
    if eof
        then return Nothing
        else fmap (Just . pack) getLine )

-- | Get command line arguments in a list
arguments :: MonadIO io => io [Text]
arguments = liftIO (fmap (map pack) getArgs)

#if MIN_VERSION_base(4,7,0)
-- | Set or modify an environment variable
export :: MonadIO io => Text -> Text -> io ()
export key val = liftIO (setEnv (unpack key) (unpack val))

-- | Delete an environment variable
unset :: MonadIO io => Text -> io ()
unset key = liftIO (unsetEnv (unpack key))
#endif

#if MIN_VERSION_base(4,6,0)
-- | Look up an environment variable
need :: MonadIO io => Text -> io (Maybe Text)
need key = liftIO (fmap (fmap pack) (lookupEnv (unpack key)))
#endif

-- | Retrieve all environment variables
env :: MonadIO io => io [(Text, Text)]
env = liftIO (fmap (fmap toTexts) getEnvironment)
  where
    toTexts (key, val) = (pack key, pack val)

-- | Change the current directory
cd :: MonadIO io => FilePath -> io ()
cd path = liftIO (Filesystem.setWorkingDirectory path)

-- | Get the current directory
pwd :: MonadIO io => io FilePath
pwd = liftIO Filesystem.getWorkingDirectory

-- | Get the home directory
home :: MonadIO io => io FilePath
home = liftIO Filesystem.getHomeDirectory

-- | Canonicalize a path
realpath :: MonadIO io => FilePath -> io FilePath
realpath path = liftIO (Filesystem.canonicalizePath path)

#ifdef mingw32_HOST_OS
fILE_ATTRIBUTE_REPARSE_POINT :: Win32.FileAttributeOrFlag
fILE_ATTRIBUTE_REPARSE_POINT = 1024

reparsePoint :: Win32.FileAttributeOrFlag -> Bool
reparsePoint attr = fILE_ATTRIBUTE_REPARSE_POINT .&. attr /= 0
#endif

{-| Stream all immediate children of the given directory, excluding @\".\"@ and
    @\"..\"@
-}
ls :: FilePath -> Shell FilePath
ls path = Shell (\(FoldM step begin done) -> do
    x0 <- begin
    let path' = Filesystem.encodeString path
    canRead <- fmap
         Directory.readable
        (Directory.getPermissions (deslash path'))
#ifdef mingw32_HOST_OS
    reparse <- fmap reparsePoint (Win32.getFileAttributes path')
    if (canRead && not reparse)
        then bracket
            (Win32.findFirstFile (Filesystem.encodeString (path </> "*")))
            (\(h, _) -> Win32.findClose h)
            (\(h, fdat) -> do
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

{-| This is used to remove the trailing slash from a path, because
    `getPermissions` will fail if a path ends with a trailing slash
-}
deslash :: String -> String
deslash []     = []
deslash (c0:cs0) = c0:go cs0
  where
    go []     = []
    go ['\\'] = []
    go (c:cs) = c:go cs

-- | Stream all recursive descendents of the given directory
lstree :: FilePath -> Shell FilePath
lstree path = do
    child <- ls path
    isDir <- testdir child
    if isDir
        then return child <|> lstree child
        else return child

{-| Stream all recursive descendents of the given directory

    This skips any directories that fail the supplied predicate

> lstree = lsif (\_ -> return True)
-}
lsif :: (FilePath -> IO Bool) -> FilePath -> Shell FilePath
lsif predicate path = do
    child <- ls path
    isDir <- testdir child
    if isDir
        then do
            continue <- liftIO (predicate child)
            if continue
                then return child <|> lsif predicate child
                else return child
        else return child

{-| Move a file or directory

    Works if the two paths are on the same filesystem.
    If not, @mv@ will still work when dealing with a regular file,
    but the operation will not be atomic
-}
mv :: MonadIO io => FilePath -> FilePath -> io ()
mv oldPath newPath = liftIO $ catchIOError (Filesystem.rename oldPath newPath)
   (\ioe -> if ioeGetErrorType ioe == UnsupportedOperation -- certainly EXDEV
                then do
                    Filesystem.copyFile oldPath newPath
                    Filesystem.removeFile oldPath
                else ioError ioe)

{-| Create a directory

    Fails if the directory is present
-}
mkdir :: MonadIO io => FilePath -> io ()
mkdir path = liftIO (Filesystem.createDirectory False path)

{-| Create a directory tree (equivalent to @mkdir -p@)

    Does not fail if the directory is present
-}
mktree :: MonadIO io => FilePath -> io ()
mktree path = liftIO (Filesystem.createTree path)

-- | Copy a file
cp :: MonadIO io => FilePath -> FilePath -> io ()
cp oldPath newPath = liftIO (Filesystem.copyFile oldPath newPath)

-- | Remove a file
rm :: MonadIO io => FilePath -> io ()
rm path = liftIO (Filesystem.removeFile path)

-- | Remove a directory
rmdir :: MonadIO io => FilePath -> io ()
rmdir path = liftIO (Filesystem.removeDirectory path)

{-| Remove a directory tree (equivalent to @rm -r@)

    Use at your own risk
-}
rmtree :: MonadIO io => FilePath -> io ()
rmtree path0 = liftIO (sh (loop path0))
  where
#ifdef mingw32_HOST_OS
    loop path = do
        isDir <- testdir path
        if isDir
            then (do
                child <- ls path
                loop child ) <|> rmdir path
            else rm path
#else
    loop path = do
        let path' = Filesystem.encodeString path
        stat <- liftIO $ getSymbolicLinkStatus path'
        let isLink = isSymbolicLink stat
            isDir = isDirectory stat
        if isLink
            then rm path
            else do
                if isDir
                    then (do
                        child <- ls path
                        loop child ) <|> rmdir path
                    else rm path
#endif

-- | Check if a file exists
testfile :: MonadIO io => FilePath -> io Bool
testfile path = liftIO (Filesystem.isFile path)

-- | Check if a directory exists
testdir :: MonadIO io => FilePath -> io Bool
testdir path = liftIO (Filesystem.isDirectory path)

-- | Check if a path exists
testpath :: MonadIO io => FilePath -> io Bool
testpath path = do
  exists <- testfile path
  if exists
    then return exists
    else testdir path

{-| Touch a file, updating the access and modification times to the current time

    Creates an empty file if it does not exist
-}
touch :: MonadIO io => FilePath -> io ()
touch file = do
    exists <- testfile file
    liftIO (if exists
#ifdef mingw32_HOST_OS
        then do
            handle <- Win32.createFile
                (Filesystem.encodeString file)
                Win32.gENERIC_WRITE
                Win32.fILE_SHARE_NONE
                Nothing
                Win32.oPEN_EXISTING
                Win32.fILE_ATTRIBUTE_NORMAL
                Nothing
            (creationTime, _, _) <- Win32.getFileTime handle
            systemTime <- Win32.getSystemTimeAsFileTime
            Win32.setFileTime handle creationTime systemTime systemTime
#else
        then touchFile (Filesystem.encodeString file)
#endif
        else output file empty )

{-| Update a file or directory's user permissions

> chmod rwo        "foo.txt"  -- chmod u=rw foo.txt
> chmod executable "foo.txt"  -- chmod u+x foo.txt
> chmod nonwritable "foo.txt" -- chmod u-x foo.txt
-}
chmod
    :: MonadIO io
    => (Permissions -> Permissions)
    -- ^ Permissions update function
    -> FilePath
    -- ^ Path
    -> io Permissions
    -- ^ Updated permissions
chmod modifyPermissions path = liftIO (do
    let path' = deslash (Filesystem.encodeString path)
    permissions <- Directory.getPermissions path'
    let permissions' = modifyPermissions permissions
        changed = permissions /= permissions'
    when changed (Directory.setPermissions path' permissions')
    return permissions' )

-- | Get a file or directory's user permissions
getmod :: MonadIO io => FilePath -> io Permissions
getmod path = liftIO (do
    let path' = deslash (Filesystem.encodeString path)
    Directory.getPermissions path' )

-- | Set a file or directory's user permissions
setmod :: MonadIO io => Permissions -> FilePath -> io ()
setmod permissions path = liftIO (do
    let path' = deslash (Filesystem.encodeString path)
    Directory.setPermissions path' permissions )

-- | Copy a file or directory's permissions (analogous to @chmod --reference@)
copymod :: MonadIO io => FilePath -> FilePath -> io ()
copymod sourcePath targetPath = liftIO (do
    let sourcePath' = deslash (Filesystem.encodeString sourcePath)
        targetPath' = deslash (Filesystem.encodeString targetPath)
    Directory.copyPermissions sourcePath' targetPath' )

-- | @+r@
readable :: Permissions -> Permissions
readable = Directory.setOwnerReadable True

-- | @-r@
nonreadable :: Permissions -> Permissions
nonreadable = Directory.setOwnerReadable False

-- | @+w@
writable :: Permissions -> Permissions
writable = Directory.setOwnerWritable True

-- | @-w@
nonwritable :: Permissions -> Permissions
nonwritable = Directory.setOwnerWritable False

-- | @+x@
executable :: Permissions -> Permissions
executable = Directory.setOwnerExecutable True

-- | @-x@
nonexecutable :: Permissions -> Permissions
nonexecutable = Directory.setOwnerExecutable False

-- | @+s@
searchable :: Permissions -> Permissions
searchable = Directory.setOwnerSearchable True

-- | @-s@
nonsearchable :: Permissions -> Permissions
nonsearchable = Directory.setOwnerSearchable False

-- | @-r -w -x@
ooo :: Permissions -> Permissions
ooo = const Directory.emptyPermissions

-- | @+r -w -x@
roo :: Permissions -> Permissions
roo = readable . ooo

-- | @-r +w -x@
owo :: Permissions -> Permissions
owo = writable . ooo

-- | @-r -w +x@
oox :: Permissions -> Permissions
oox = executable . ooo

-- | @-r -w +s@
oos :: Permissions -> Permissions
oos = searchable . ooo

-- | @+r +w -x@
rwo :: Permissions -> Permissions
rwo = readable . writable . ooo

-- | @+r -w +x@
rox :: Permissions -> Permissions
rox = readable . executable . ooo

-- | @+r -w +s@
ros :: Permissions -> Permissions
ros = readable . searchable . ooo

-- | @-r +w +x@
owx :: Permissions -> Permissions
owx = writable . executable . ooo

-- | @+r +w +x@
rwx :: Permissions -> Permissions
rwx = readable . writable . executable . ooo

-- | @+r +w +s@
rws :: Permissions -> Permissions
rws = readable . writable . searchable . ooo

{-| Time how long a command takes in monotonic wall clock time

    Returns the duration alongside the return value
-}
time :: MonadIO io => io a -> io (a, NominalDiffTime)
time io = do
    TimeSpec seconds1 nanoseconds1 <- liftIO (getTime Monotonic)
    a <- io
    TimeSpec seconds2 nanoseconds2 <- liftIO (getTime Monotonic)
    let t = fromIntegral (    seconds2 -     seconds1)
          + fromIntegral (nanoseconds2 - nanoseconds1) / 10^(9::Int)
    return (a, fromRational t)

-- | Get the system's host name
hostname :: MonadIO io => io Text
hostname = liftIO (fmap Text.pack getHostName)

{-| Sleep for the given duration

    A numeric literal argument is interpreted as seconds.  In other words,
    @(sleep 2.0)@ will sleep for two seconds.
-}
sleep :: MonadIO io => NominalDiffTime -> io ()
sleep n = liftIO (threadDelay (truncate (n * 10^(6::Int))))

{-| Exit with the given exit code

    An exit code of @0@ indicates success
-}
exit :: MonadIO io => ExitCode -> io a
exit code = liftIO (exitWith code)

-- | Throw an exception using the provided `Text` message
die :: MonadIO io => Text -> io a
die txt = liftIO (throwIO (userError (unpack txt)))

infixr 2 .||.
infixr 3 .&&.

{-| Analogous to `&&` in Bash

    Runs the second command only if the first one returns `ExitSuccess`
-}
(.&&.) :: Monad m => m ExitCode -> m ExitCode -> m ExitCode
cmd1 .&&. cmd2 = do
    r <- cmd1
    case r of
        ExitSuccess -> cmd2
        _           -> return r

{-| Analogous to `||` in Bash

    Run the second command only if the first one returns `ExitFailure`
-}
(.||.) :: Monad m => m ExitCode -> m ExitCode -> m ExitCode
cmd1 .||. cmd2 = do
    r <- cmd1
    case r of
        ExitFailure _ -> cmd2
        _             -> return r

{-| Create a temporary directory underneath the given directory

    Deletes the temporary directory when done
-}
mktempdir
    :: FilePath
    -- ^ Parent directory
    -> Text
    -- ^ Directory name template
    -> Managed FilePath
mktempdir parent prefix = do
    let parent' = Filesystem.encodeString parent
    let prefix' = unpack prefix
    dir' <- managed (withTempDirectory parent' prefix')
    return (Filesystem.decodeString dir')

{-| Create a temporary file underneath the given directory

    Deletes the temporary file when done

    Note that this provides the `Handle` of the file in order to avoid a
    potential race condition from the file being moved or deleted before you
    have a chance to open the file.  The `mktempfile` function provides a
    simpler API if you don't need to worry about that possibility.
-}
mktemp
    :: FilePath
    -- ^ Parent directory
    -> Text
    -- ^ File name template
    -> Managed (FilePath, Handle)
mktemp parent prefix = do
    let parent' = Filesystem.encodeString parent
    let prefix' = unpack prefix
    (file', handle) <- managed (\k ->
        withTempFile parent' prefix' (\file' handle -> k (file', handle)) )
    return (Filesystem.decodeString file', handle)

{-| Create a temporary file underneath the given directory

    Deletes the temporary file when done
-}
mktempfile
    :: FilePath
    -- ^ Parent directory
    -> Text
    -- ^ File name template
    -> Managed FilePath
mktempfile parent prefix = do
    let parent' = Filesystem.encodeString parent
    let prefix' = unpack prefix
    (file', handle) <- managed (\k ->
        withTempFile parent' prefix' (\file' handle -> k (file', handle)) )
    liftIO (hClose handle)
    return (Filesystem.decodeString file')

-- | Fork a thread, acquiring an `Async` value
fork :: IO a -> Managed (Async a)
fork io = managed (withAsync io)

-- | Read lines of `Text` from standard input
stdin :: Shell Text
stdin = inhandle IO.stdin

-- | Read lines of `Text` from a file
input :: FilePath -> Shell Text
input file = do
    handle <- using (readonly file)
    inhandle handle

-- | Read lines of `Text` from a `Handle`
inhandle :: Handle -> Shell Text
inhandle handle = Shell (\(FoldM step begin done) -> do
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

-- | Stream lines of `Text` to standard output
stdout :: MonadIO io => Shell Text -> io ()
stdout s = sh (do
    txt <- s
    liftIO (echo txt) )

-- | Stream lines of `Text` to a file
output :: MonadIO io => FilePath -> Shell Text -> io ()
output file s = sh (do
    handle <- using (writeonly file)
    txt    <- s
    liftIO (Text.hPutStrLn handle txt) )

-- | Stream lines of `Text` to a `Handle`
outhandle :: MonadIO io => Handle -> Shell Text -> io ()
outhandle handle s = sh (do
    txt <- s
    liftIO (Text.hPutStrLn handle txt) )

-- | Stream lines of `Text` to append to a file
append :: MonadIO io => FilePath -> Shell Text -> io ()
append file s = sh (do
    handle <- using (appendonly file)
    txt    <- s
    liftIO (Text.hPutStrLn handle txt) )

-- | Stream lines of `Text` to standard error
stderr :: MonadIO io => Shell Text -> io ()
stderr s = sh (do
    txt <- s
    liftIO (err txt) )

-- | Read in a stream's contents strictly
strict :: MonadIO io => Shell Text -> io Text
strict s = liftM Text.unlines (fold s list)

-- | Acquire a `Managed` read-only `Handle` from a `FilePath`
readonly :: FilePath -> Managed Handle
readonly file = managed (Filesystem.withTextFile file IO.ReadMode)

-- | Acquire a `Managed` write-only `Handle` from a `FilePath`
writeonly :: FilePath -> Managed Handle
writeonly file = managed (Filesystem.withTextFile file IO.WriteMode)

-- | Acquire a `Managed` append-only `Handle` from a `FilePath`
appendonly :: FilePath -> Managed Handle
appendonly file = managed (Filesystem.withTextFile file IO.AppendMode)

-- | Combine the output of multiple `Shell`s, in order
cat :: [Shell a] -> Shell a
cat = msum

-- | Keep all lines that match the given `Pattern`
grep :: Pattern a -> Shell Text -> Shell Text
grep pattern s = do
    txt <- s
    _:_ <- return (match pattern txt)
    return txt

{-| Replace all occurrences of a `Pattern` with its `Text` result

    `sed` performs substitution on a line-by-line basis, meaning that
    substitutions may not span multiple lines.  Additionally, substitutions may
    occur multiple times within the same line, like the behavior of
    @s/.../.../g@.

    Warning: Do not use a `Pattern` that matches the empty string, since it will
    match an infinite number of times.  `sed` tries to detect such `Pattern`s
    and `die` with an error message if they occur, but this detection is
    necessarily incomplete.
-}
sed :: Pattern Text -> Shell Text -> Shell Text
sed pattern s = do
    when (matchesEmpty pattern) (die message)
    let pattern' = fmap Text.concat
            (many (pattern <|> fmap Text.singleton anyChar))
    txt    <- s
    txt':_ <- return (match pattern' txt)
    return txt'
  where
    message = "sed: the given pattern matches the empty string"
    matchesEmpty = not . null . flip match ""

-- | Like `sed`, but operates in place on a `FilePath` (analogous to @sed -i@)
inplace :: MonadIO io => Pattern Text -> FilePath -> io ()
inplace pattern file = liftIO (runManaged (do
    here              <- pwd
    (tmpfile, handle) <- mktemp here "turtle"
    outhandle handle (sed pattern (input file))
    liftIO (hClose handle)
    copymod file tmpfile
    mv tmpfile file ))


-- | Search a directory recursively for all files matching the given `Pattern`
find :: Pattern a -> FilePath -> Shell FilePath
find pattern dir = do
    path <- lstree dir
    Right txt <- return (Filesystem.toText path)
    _:_       <- return (match pattern txt)
    return path

-- | A Stream of @\"y\"@s
yes :: Shell Text
yes = fmap (\_ -> "y") endless

-- | Number each element of a `Shell` (starting at 0)
nl :: Num n => Shell a -> Shell (n, a)
nl s = Shell _foldIO'
  where
    _foldIO' (FoldM step begin done) = _foldIO s (FoldM step' begin' done')
      where
        step' (x, n) a = do
            x' <- step x (n, a)
            let n' = n + 1
            n' `seq` return (x', n')
        begin' = do
            x0 <- begin
            return (x0, 0)
        done' (x, _) = done x

data ZipState a b = Empty | HasA a | HasAB a b | Done

{-| Merge two `Shell`s together, element-wise

    If one `Shell` is longer than the other, the excess elements are
    truncated
-}
paste :: Shell a -> Shell b -> Shell (a, b)
paste sA sB = Shell _foldIOAB
  where
    _foldIOAB (FoldM stepAB beginAB doneAB) = do
        x0 <- beginAB

        tvar <- STM.atomically (STM.newTVar Empty)

        let begin = return ()

        let stepA () a = STM.atomically (do
                x <- STM.readTVar tvar
                case x of
                    Empty -> STM.writeTVar tvar (HasA a)
                    Done  -> return ()
                    _     -> STM.retry )
        let doneA () = STM.atomically (do
                x <- STM.readTVar tvar
                case x of
                    Empty -> STM.writeTVar tvar Done
                    Done  -> return ()
                    _     -> STM.retry )
        let foldA = FoldM stepA begin doneA

        let stepB () b = STM.atomically (do
                x <- STM.readTVar tvar
                case x of
                    HasA a -> STM.writeTVar tvar (HasAB a b)
                    Done   -> return ()
                    _      -> STM.retry )
        let doneB () = STM.atomically (do
                x <- STM.readTVar tvar
                case x of
                    HasA _ -> STM.writeTVar tvar Done
                    Done   -> return ()
                    _      -> STM.retry )
        let foldB = FoldM stepB begin doneB

        withAsync (foldIO sA foldA) (\asyncA -> do
            withAsync (foldIO sB foldB) (\asyncB -> do
                let loop x = do
                        y <- STM.atomically (do
                            z <- STM.readTVar tvar
                            case z of
                                HasAB a b -> do
                                    STM.writeTVar tvar Empty
                                    return (Just (a, b))
                                Done      -> return  Nothing
                                _         -> STM.retry )
                        case y of
                            Nothing -> return x
                            Just ab -> do
                                x' <- stepAB x ab
                                loop $! x'
                x' <- loop $! x0
                wait asyncA
                wait asyncB
                doneAB x' ) )

-- | A `Shell` that endlessly emits @()@
endless :: Shell ()
endless = Shell (\(FoldM step begin _) -> do
    x0 <- begin
    let loop x = do
            x' <- step x ()
            loop $! x'
    loop $! x0 )

-- | Limit a `Shell` to a fixed number of values
limit :: Int -> Shell a -> Shell a
limit n s = Shell (\(FoldM step begin done) -> do
    ref <- newIORef 0  -- I feel so dirty
    let step' x a = do
            n' <- readIORef ref
            writeIORef ref (n' + 1)
            if n' < n then step x a else return x
    foldIO s (FoldM step' begin done) )

{-| Limit a `Shell` to values that satisfy the predicate

    This terminates the stream on the first value that does not satisfy the
    predicate
-}
limitWhile :: (a -> Bool) -> Shell a -> Shell a
limitWhile predicate s = Shell (\(FoldM step begin done) -> do
    ref <- newIORef True
    let step' x a = do
            b <- readIORef ref
            let b' = b && predicate a
            writeIORef ref b'
            if b' then step x a else return x
    foldIO s (FoldM step' begin done) )

{-| Cache a `Shell`'s output so that repeated runs of the script will reuse the
    result of previous runs.  You must supply a `FilePath` where the cached
    result will be stored.

    The stored result is only reused if the `Shell` successfully ran to
    completion without any exceptions.  Note: on some platforms Ctrl-C will
    flush standard input and signal end of file before killing the program,
    which may trick the program into \"successfully\" completing.
-}
cache :: (Read a, Show a) => FilePath -> Shell a -> Shell a
cache file s = do
    let cached = do
            txt <- input file
            case reads (Text.unpack txt) of
                [(ma, "")] -> return ma
                _          ->
                    die (format ("cache: Invalid data stored in "%w) file)
    exists <- testfile file
    mas    <- fold (if exists then cached else empty) list
    case [ () | Nothing <- mas ] of
        _:_ -> select [ a | Just a <- mas ]
        _   -> do
            handle <- using (writeonly file)
            let justs = do
                    a      <- s
                    liftIO (Text.hPutStrLn handle (Text.pack (show (Just a))))
                    return a
            let nothing = do
                    let n = Nothing :: Maybe ()
                    liftIO (Text.hPutStrLn handle (Text.pack (show n)))
                    empty
            justs <|> nothing

-- | Split a line into chunks delimited by the given `Pattern`
cut :: Pattern a -> Text -> [Text]
cut pattern txt = head (match (selfless chars `sepBy` pattern) txt)
-- This `head` should be safe ... in theory

-- | Get the current time
date :: MonadIO io => io UTCTime
date = liftIO getCurrentTime

-- | Get the time a file was last modified
datefile :: MonadIO io => FilePath -> io UTCTime
datefile path = liftIO (Filesystem.getModified path)

-- | Get the size of a file or a directory
du :: MonadIO io => FilePath -> io Size
du path = liftIO (fmap Size (Filesystem.getSize path))

{-| An abstract file size

    Specify the units you want by using an accessor like `kilobytes`

    The `Num` instance for `Size` interprets numeric literals as bytes
-}
newtype Size = Size { _bytes :: Integer } deriving (Num)

instance Show Size where
    show = show . _bytes

{-| `Format` a `Size` using a human readable representation

>>> format sz 42
"42 B"
>>> format sz 2309
"2.309 KB"
>>> format sz 949203
"949.203 MB"
>>> format sz 1600000000
"1.600 GB"
>>> format sz 999999999999999999
"999999.999 TB"
-}
sz :: Format r (Size -> r)
sz = makeFormat (\(Size numBytes) ->
    let (numKilobytes, remainingBytes    ) = numBytes     `quotRem` 1000
        (numMegabytes, remainingKilobytes) = numKilobytes `quotRem` 1000
        (numGigabytes, remainingMegabytes) = numMegabytes `quotRem` 1000
        (numTerabytes, remainingGigabytes) = numGigabytes `quotRem` 1000
    in  if numKilobytes <= 0
        then format (d%" B" ) remainingBytes
        else if numMegabytes == 0
        then format (d%"."%d%" KB") remainingKilobytes remainingBytes
        else if numGigabytes == 0
        then format (d%"."%d%" MB") remainingMegabytes remainingKilobytes
        else if numTerabytes == 0
        then format (d%"."%d%" GB") remainingGigabytes remainingMegabytes
        else format (d%"."%d%" TB") numTerabytes       remainingGigabytes )

-- | Extract a size in bytes
bytes :: Integral n => Size -> n
bytes = fromInteger . _bytes

-- | @1 kilobyte = 1000 bytes@
kilobytes :: Integral n => Size -> n
kilobytes = (`div` 1000) . bytes

-- | @1 megabyte = 1000 kilobytes@
megabytes :: Integral n => Size -> n
megabytes = (`div` 1000) . kilobytes

-- | @1 gigabyte = 1000 megabytes@
gigabytes :: Integral n => Size -> n
gigabytes = (`div` 1000) . megabytes

-- | @1 terabyte = 1000 gigabytes@
terabytes :: Integral n => Size -> n
terabytes = (`div` 1000) . gigabytes

-- | @1 kibibyte = 1024 bytes@
kibibytes :: Integral n => Size -> n
kibibytes = (`div` 1024) . bytes

-- | @1 mebibyte = 1024 kibibytes@
mebibytes :: Integral n => Size -> n
mebibytes = (`div` 1024) . kibibytes

-- | @1 gibibyte = 1024 mebibytes@
gibibytes :: Integral n => Size -> n
gibibytes = (`div` 1024) . mebibytes

-- | @1 tebibyte = 1024 gibibytes@
tebibytes :: Integral n => Size -> n
tebibytes = (`div` 1024) . gibibytes

{-| Count the number of characters in the stream (like @wc -c@)

    This uses the convention that the elements of the stream are implicitly
    ended by newlines that are one character wide
-}
countChars :: Integral n => Fold Text n
countChars = Control.Foldl.Text.length + charsPerNewline * countLines

charsPerNewline :: Num a => a
#ifdef mingw32_HOST_OS
charsPerNewline = 2
#else
charsPerNewline = 1
#endif

-- | Count the number of words in the stream (like @wc -w@)
countWords :: Integral n => Fold Text n
countWords = premap Text.words (handles traverse genericLength)

{-| Count the number of lines in the stream (like @wc -l@)

    This uses the convention that each element of the stream represents one
    line
-}
countLines :: Integral n => Fold Text n
countLines = genericLength
