{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

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
-- >>> view (find "Browser.py" "/usr/lib")
-- FilePath "lib/python3.2/idlelib/ObjectBrowser.py"
-- FilePath "lib/python3.2/idlelib/PathBrowser.py"
-- FilePath "lib/python3.2/idlelib/RemoteObjectBrowser.py"
-- FilePath "lib/python3.2/idlelib/ClassBrowser.py"
--
-- Use `fold` to reduce the output of a `Shell` stream:
--
-- >>> import qualified Control.Foldl as Fold
-- >>> fold (ls "/usr") Fold.length
-- 8
-- >>> fold (find "Browser.py" "/usr/lib") Fold.head
-- Just (FilePath "/usr/lib/python3.2/idlelib/ObjectBrowser.py")
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
      proc
    , shell
    , procStrict
    , shellStrict
    , echo
    , err
    , readline
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
    , du
    , testfile
    , testdir
    , date
    , datefile
    , touch
    , time
    , sleep
    , exit
    , die
    , (.&&.)
    , (.||.)

    -- * Permissions
    , Permissions
    , chmod
    , readable, nonreadable
    , writable, nonwritable
    , executable, nonexecutable
    , searchable, nonsearchable
    , ooo,roo,owo,oox,oos,rwo,rox,ros,owx,rwx,rws

    -- * Managed
    , readonly
    , writeonly
    , appendonly
    , mktemp
    , mktempdir
    , fork
    , wait

    -- * Shell
    , inproc
    , inshell
    , stdin
    , input
    , inhandle
    , stdout
    , stderr
    , output
    , append
    , strict
    , ls
    , lstree
    , cat
    , grep
    , sed
    , find
    , yes
    , limit
    , limitWhile
    ) where

import Control.Applicative (Alternative(..))
import Control.Concurrent.Async (Async, withAsync, wait, concurrently)
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, throwIO)
import Control.Foldl (FoldM(..), list)
import Control.Monad (liftM, msum, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Managed (Managed, managed)
#ifdef mingw32_HOST_OS
import Data.Bits ((.&.))
#endif
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text, pack, unpack)
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import qualified Data.Text    as Text
import qualified Data.Text.IO as Text
import qualified Filesystem
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as Filesystem
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
import System.IO (Handle)
import qualified System.IO as IO
import System.IO.Temp (withTempDirectory, withTempFile)
import qualified System.Process as Process
#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#else
import System.Posix (openDirStream, readDirStream, closeDirStream, touchFile)
#endif
import Prelude hiding (FilePath)

import Turtle.Pattern (Pattern, anyChar, match)
import Turtle.Shell

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
proc cmd args = system (Process.proc (unpack cmd) (map unpack args))

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
shell cmdLine = system (Process.shell (unpack cmdLine))

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

system
    :: MonadIO io
    => Process.CreateProcess
    -- ^ Command
    -> Shell Text
    -- ^ Lines of standard input
    -> io ExitCode
    -- ^ Exit code
system p s = liftIO (do
    let p' = p
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            }
    (Just hIn, Nothing, Nothing, ph) <- liftIO (Process.createProcess p')
    let feedIn = sh (do
            txt <- s
            liftIO (Text.hPutStrLn hIn txt) )
    withAsync feedIn (\_ -> liftIO (Process.waitForProcess ph) ) )

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
    (Just hIn, Just hOut, Nothing, ph) <- liftIO (Process.createProcess p')
    let feedIn = sh (do
            txt <- s
            liftIO (Text.hPutStrLn hIn txt) )
    concurrently
        (withAsync feedIn (\_ -> liftIO (Process.waitForProcess ph) ))
        (Text.hGetContents hOut) )

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
    (Just hIn, Just hOut, Nothing, _) <- liftIO (Process.createProcess p')
    let feedIn = sh (do
            txt <- s
            liftIO (Text.hPutStrLn hIn txt) )
    _ <- using (fork feedIn)
    inhandle hOut

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
    isDir <- liftIO (testdir child)
    if isDir
        then return child <|> lstree child
        else return child

-- | Move a file or directory
mv :: MonadIO io => FilePath -> FilePath -> io ()
mv oldPath newPath = liftIO (Filesystem.rename oldPath newPath)

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
rmtree path = liftIO (Filesystem.removeTree path)

-- | Get a file or directory's size
du :: MonadIO io => FilePath -> io Integer
du path = liftIO (Filesystem.getSize path)

-- | Check if a file exists
testfile :: MonadIO io => FilePath -> io Bool
testfile path = liftIO (Filesystem.isFile path)

-- | Check if a directory exists
testdir :: MonadIO io => FilePath -> io Bool
testdir path = liftIO (Filesystem.isDirectory path)

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

{-| Update a file or directory's permissions

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
    -> io (Bool, Permissions)
    -- ^ Updated permissions
chmod modifyPermissions path = liftIO (do
    let path' = deslash (Filesystem.encodeString path)
    permissions <- Directory.getPermissions path'
    let permissions' = modifyPermissions permissions
        changed = permissions /= permissions'
    when changed (Directory.setPermissions path' permissions')
    return (changed, permissions') )

readable, nonreadable :: Permissions -> Permissions
readable = Directory.setOwnerReadable True
nonreadable = Directory.setOwnerReadable False

writable, nonwritable :: Permissions -> Permissions
writable = Directory.setOwnerWritable True
nonwritable = Directory.setOwnerWritable False

executable, nonexecutable :: Permissions -> Permissions
executable = Directory.setOwnerExecutable True
nonexecutable = Directory.setOwnerExecutable False

searchable, nonsearchable :: Permissions -> Permissions
searchable = Directory.setOwnerSearchable True
nonsearchable = Directory.setOwnerSearchable False

ooo,roo,owo,oox,oos,rwo,rox,ros,owx,rwx,rws :: Permissions -> Permissions
ooo = const Directory.emptyPermissions
roo = readable . ooo
owo = writable . ooo
oox = executable . ooo
oos = searchable . ooo
rwo = readable . writable . ooo
rox = readable . executable . ooo
ros = readable . searchable . ooo
owx = writable . executable . ooo
rwx = readable . writable . executable . ooo
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

infixr 2 .&&., .||.

{-| Analogous to `&&` in Bash

    Runs the second command only if the first one returns `ExitSuccess`
-}
(.&&.) :: IO ExitCode -> IO ExitCode -> IO ExitCode
cmd1 .&&. cmd2 = do
    r <- cmd1
    case r of
        ExitSuccess -> cmd2
        _           -> return r

{-| Analogous to `||` in Bash

    Run the second command only if the first one returns `ExitFailure`
-}
(.||.) :: IO ExitCode -> IO ExitCode -> IO ExitCode
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
    let file = Filesystem.decodeString file'
    return (file, handle)

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

-- | Stream lines of `Text` to standard error
stderr :: MonadIO io => Shell Text -> io ()
stderr s = sh (do
    txt <- s
    liftIO (err txt) )

-- | Stream lines of `Text` to a file
output :: MonadIO io => FilePath -> Shell Text -> io ()
output file s = sh (do
    handle <- using (writeonly file)
    txt    <- s
    liftIO (Text.hPutStrLn handle txt) )

-- | Stream lines of `Text` to append to a file
append :: MonadIO io => FilePath -> Shell Text -> io ()
append file s = sh (do
    handle <- using (appendonly file)
    txt    <- s
    liftIO (Text.hPutStrLn handle txt) )

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

    Warning: Do not use a `Pattern` that matches the empty string, since it will
    match an infinite number of times
-}
sed :: Pattern Text -> Shell Text -> Shell Text
sed pattern s = do
    let pattern' = fmap Text.concat
            (many (pattern <|> fmap Text.singleton anyChar))
    txt    <- s
    txt':_ <- return (match pattern' txt)
    return txt'

-- | Search a directory recursively for all files matching the given `Pattern`
find :: Pattern a -> FilePath -> Shell FilePath
find pattern dir = do
    path <- lstree dir
    Right txt <- return (Filesystem.toText path)
    _:_       <- return (match pattern txt)
    return path

-- | A Stream of @\"y\"@s
yes :: Shell Text
yes = Shell (\(FoldM step begin _) -> do
    x0 <- begin
    let loop x = do
            x' <- step x "y"
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

-- | Get the current time
date :: MonadIO io => io UTCTime
date = liftIO getCurrentTime

-- | Get the time a file was last modified
datefile :: MonadIO io => FilePath -> io UTCTime
datefile path = liftIO (Filesystem.getModified path)
