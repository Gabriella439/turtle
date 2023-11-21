{-# LANGUAGE RankNTypes #-}

{-| This module provides `ByteString.Lazy` analogs of several utilities in
 "Turtle.Prelude". The main difference is that a there is no chunks here,
 the whole content is lazily read.
-}

module Turtle.Bytes.Lazy (
    -- * Byte operations
      stdin
    , input
    , inhandle
    , stdout
    , output
    , outhandle
    , append
    , stderr

    -- * Subprocess management
    , proc
    , shell
    , procs
    , shells
    , procStream
    , shellStream
    , procStreamWithErr
    , shellStreamWithErr

    , system
    , stream
    , streamWithErr
    ) where

import Control.Applicative
import Control.Concurrent.Async (Async, Concurrently(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Managed (MonadManaged(..))
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Filesystem.Path (FilePath)
import Prelude hiding (FilePath)
import System.Exit (ExitCode(..))
import System.IO (Handle)
import Turtle.Internal (ignoreSIGPIPE)
import Turtle.Prelude (ProcFailed(..), ShellFailed(..))
import Turtle.Shell (Shell(..), sh)

import qualified Control.Concurrent.Async      as Async
import qualified Control.Concurrent.MVar       as MVar
import qualified Control.Exception             as Exception
import qualified Control.Monad
import qualified Data.ByteString.Lazy
import qualified Data.Text
import qualified System.IO
import qualified System.Process                as Process
import qualified Turtle.Prelude

{-| Read lazily bytes from standard input

-}
stdin :: MonadIO io => io ByteString
stdin = inhandle System.IO.stdin

{-| Read chunks of bytes from a file

    The chunks are not necessarily aligned to line boundaries
-}
input :: MonadManaged managed => FilePath -> managed ByteString
input file = do
    handle <- using (Turtle.Prelude.readonly file)
    inhandle handle

{-| Read chunks of bytes from a `Handle`

    The chunks are not necessarily aligned to line boundaries
-}
inhandle :: MonadIO io => Handle -> io ByteString
inhandle = liftIO . Data.ByteString.Lazy.hGetContents

{-| Stream chunks of bytes to standard output

    The chunks are not necessarily aligned to line boundaries
-}
stdout :: MonadIO io => Shell ByteString -> io ()
stdout s = sh (do
    bytes <- s
    liftIO (Data.ByteString.Lazy.hPut System.IO.stdout bytes) )

{-| Stream chunks of bytes to a file

    The chunks do not need to be aligned to line boundaries
-}
output :: MonadIO io => FilePath -> Shell ByteString -> io ()
output file s = sh (do
    handle <- using (Turtle.Prelude.writeonly file)
    bytes  <- s
    liftIO (Data.ByteString.Lazy.hPut handle bytes) )

{-| Stream chunks of bytes to a `Handle`

    The chunks do not need to be aligned to line boundaries
-}
outhandle :: MonadIO io => Handle -> Shell ByteString -> io ()
outhandle handle s = sh (do
    bytes <- s
    liftIO (Data.ByteString.Lazy.hPut handle bytes) )

{-| Append chunks of bytes to append to a file

    The chunks do not need to be aligned to line boundaries
-}
append :: MonadIO io => FilePath -> Shell ByteString -> io ()
append file s = sh (do
    handle <- using (Turtle.Prelude.appendonly file)
    bytes  <- s
    liftIO (Data.ByteString.Lazy.hPut handle bytes) )

{-| Stream chunks of bytes to standard error

    The chunks do not need to be aligned to line boundaries
-}
stderr :: MonadIO io => Shell ByteString -> io ()
stderr s = sh (do
    bytes <- s
    liftIO (Data.ByteString.Lazy.hPut System.IO.stderr bytes) )

{-| Run a command using @execvp@, retrieving the exit code

    The command inherits @stdout@ and @stderr@ for the current process
-}
proc
    :: MonadIO io
    => Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io ExitCode
    -- ^ Exit code
proc cmd args =
    system
        ( (Process.proc (Data.Text.unpack cmd) (map Data.Text.unpack args))
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
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io ExitCode
    -- ^ Exit code
shell cmdline =
    system
        ( (Process.shell (Data.Text.unpack cmdline))
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.Inherit
            , Process.std_err = Process.Inherit
            } )

{-| This function is identical to `proc` except this throws `ProcFailed` for
    non-zero exit codes
-}
procs
    :: MonadIO io
    => Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io ()
procs cmd args s = do
    exitCode <- proc cmd args s
    case exitCode of
        ExitSuccess -> return ()
        _           -> liftIO (Exception.throwIO (ProcFailed cmd args exitCode))

{-| This function is identical to `shell` except this throws `ShellFailed` for
    non-zero exit codes
-}
shells
    :: MonadIO io
    => Text
    -- ^ Command line
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io ()
    -- ^ Exit code
shells cmdline s = do
    exitCode <- shell cmdline s
    case exitCode of
        ExitSuccess -> return ()
        _           -> liftIO (Exception.throwIO (ShellFailed cmdline exitCode))

{-| Run a command using @execvp@, retrieving the exit code and stdout as a
 lazy `ByteString`

    The command inherits @stderr@ for the current process
-}
procStream
    :: MonadIO io
    => Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io (ExitCode, ByteString)
    -- ^ Exit code and stdout
procStream cmd args =
    stream (Process.proc (Data.Text.unpack cmd) (map Data.Text.unpack args))

{-| Run a command line using the shell, retrieving the exit code and stdout as a
    lazy 'ByteString'

    This command is more powerful than `proc`, but highly vulnerable to code
    injection if you template the command line with untrusted input

    The command inherits @stderr@ for the current process
-}
shellStream
    :: MonadIO io
    => Text
    -- ^ Command line
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io (ExitCode, ByteString)
    -- ^ Exit code and stdout
shellStream cmdline = stream (Process.shell (Data.Text.unpack cmdline))

{-| Run a command using @execvp@, retrieving the exit code, stdout, and stderr
    as a lazy 'ByteString'
-}
procStreamWithErr
    :: MonadIO io
    => Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io (ExitCode, ByteString, ByteString)
    -- ^ (Exit code, stdout, stderr)
procStreamWithErr cmd args =
    streamWithErr (Process.proc (Data.Text.unpack cmd) (map Data.Text.unpack args))

{-| Run a command line using the shell, retrieving the exit code, stdout, and
 stderr as a lazy `ByteString`

    This command is more powerful than `proc`, but highly vulnerable to code
    injection if you template the command line with untrusted input
-}
shellStreamWithErr
    :: MonadIO io
    => Text
    -- ^ Command line
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io (ExitCode, ByteString, ByteString)
    -- ^ (Exit code, stdout, stderr)
shellStreamWithErr cmdline =
    streamWithErr (Process.shell (Data.Text.unpack cmdline))

-- | Halt an `Async` thread, re-raising any exceptions it might have thrown
halt :: Async a -> IO ()
halt a = do
    m <- Async.poll a
    case m of
        Nothing        -> Async.cancel a
        Just (Left  e) -> Exception.throwIO e
        Just (Right _) -> return ()

{-| `system` generalizes `shell` and `proc` by allowing you to supply your own
    custom `CreateProcess`.  This is for advanced users who feel comfortable
    using the lower-level @process@ API
-}
system
    :: MonadIO io
    => Process.CreateProcess
    -- ^ Command
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io ExitCode
    -- ^ Exit code
system p s = liftIO (do
    let open = do
            (m, Nothing, Nothing, ph) <- Process.createProcess p
            case m of
                Just hIn -> System.IO.hSetBuffering hIn (System.IO.BlockBuffering Nothing)
                _        -> return ()
            return (m, ph)

    -- Prevent double close
    mvar <- MVar.newMVar False
    let close handle = do
            MVar.modifyMVar_ mvar (\finalized -> do
                Control.Monad.unless finalized
                    (ignoreSIGPIPE (System.IO.hClose handle))
                return True )
    let close' (Just hIn, ph) = do
            close hIn
            Process.terminateProcess ph
        close' (Nothing , ph) = do
            Process.terminateProcess ph

    let handle (Just hIn, ph) = do
            let feedIn :: (forall a. IO a -> IO a) -> IO ()
                feedIn restore =
                    restore (ignoreSIGPIPE (outhandle hIn s))
                    `Exception.finally` close hIn
            Exception.mask (\restore ->
                Async.withAsync (feedIn restore) (\a ->
                    restore (Process.waitForProcess ph) <* halt a ) )
        handle (Nothing , ph) = do
            Process.waitForProcess ph

    Exception.bracket open close' handle )

{-| `stream` generalizes `inproc` and `inshell` by allowing you to supply your
    own custom `CreateProcess`.  This is for advanced users who feel comfortable
    using the lower-level @process@ API

    Throws an `ExitCode` exception if the command returns a non-zero exit code
-}
stream
    :: MonadIO io
    => Process.CreateProcess
    -- ^ Command
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io (ExitCode, ByteString)
    -- ^ Exit code and stdout
stream p s = liftIO (do
    let p' = p
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.Inherit
            }

    let open = do
            (Just hIn, Just hOut, Nothing, ph) <- liftIO (Process.createProcess p')
            System.IO.hSetBuffering hIn (System.IO.BlockBuffering Nothing)
            return (hIn, hOut, ph)

    -- Prevent double close
    mvar <- MVar.newMVar False
    let close handle = do
            MVar.modifyMVar_ mvar (\finalized -> do
                Control.Monad.unless finalized
                    (ignoreSIGPIPE (System.IO.hClose handle))
                return True )

    Exception.bracket open (\(hIn, _, ph) -> close hIn >> Process.terminateProcess ph) (\(hIn, hOut, ph) -> do
        let feedIn :: (forall a. IO a -> IO a) -> IO ()
            feedIn restore =
                restore (ignoreSIGPIPE (outhandle hIn s))
                `Exception.finally` close hIn

        Async.concurrently
            (Exception.mask (\restore ->
                Async.withAsync (feedIn restore) (\a ->
                    restore (Process.waitForProcess ph) <* halt a ) ))
            (Data.ByteString.Lazy.hGetContents hOut) ) )

{-| `streamWithErr` generalizes `inprocWithErr` and `inshellWithErr` by allowing
    you to supply your own custom `CreateProcess`.  This is for advanced users
    who feel comfortable using the lower-level @process@ API

    Throws an `ExitCode` exception if the command returns a non-zero exit code
-}
streamWithErr
    :: MonadIO io
    => Process.CreateProcess
    -- ^ Command
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io (ExitCode, ByteString, ByteString)
    -- ^ Exit code and stdout
streamWithErr p s = liftIO (do
    let p' = p
            { Process.std_in  = Process.CreatePipe
            , Process.std_out = Process.CreatePipe
            , Process.std_err = Process.CreatePipe
            }

    let open = do
            (Just hIn, Just hOut, Just hErr, ph) <- liftIO (Process.createProcess p')
            System.IO.hSetBuffering hIn (System.IO.BlockBuffering Nothing)
            return (hIn, hOut, hErr, ph)

    -- Prevent double close
    mvar <- MVar.newMVar False
    let close handle = do
            MVar.modifyMVar_ mvar (\finalized -> do
                Control.Monad.unless finalized
                    (ignoreSIGPIPE (System.IO.hClose handle))
                return True )

    Exception.bracket open (\(hIn, _, _, ph) -> close hIn >> Process.terminateProcess ph) (\(hIn, hOut, hErr, ph) -> do
        let feedIn :: (forall a. IO a -> IO a) -> IO ()
            feedIn restore =
                restore (ignoreSIGPIPE (outhandle hIn s))
                `Exception.finally` close hIn

        runConcurrently $ (,,)
            <$> Concurrently (Exception.mask (\restore ->
                    Async.withAsync (feedIn restore) (\a ->
                        restore (Process.waitForProcess ph) <* halt a ) ))
            <*> Concurrently (Data.ByteString.Lazy.hGetContents hOut)
            <*> Concurrently (Data.ByteString.Lazy.hGetContents hErr) ) )
