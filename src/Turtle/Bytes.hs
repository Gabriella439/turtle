{-# LANGUAGE RankNTypes #-}

{-| This module provides `ByteString` analogs of several utilities in
    "Turtle.Prelude".  The main difference is that the chunks of bytes read by
    these utilities are not necessarily aligned to line boundaries.
-}

module Turtle.Bytes (
    -- * Byte operations
      stdin
    , input
    , inhandle
    , stdout
    , output
    , outhandle
    , append
    , stderr
    , strict
    , compress
    , decompress
    , WindowBits(..)
    , Zlib.defaultWindowBits
    , fromUTF8
    , toUTF8

    -- * Subprocess management
    , proc
    , shell
    , procs
    , shells
    , inproc
    , inshell
    , inprocWithErr
    , inshellWithErr
    , procStrict
    , shellStrict
    , procStrictWithErr
    , shellStrictWithErr

    , system
    , stream
    , streamWithErr
    , systemStrict
    , systemStrictWithErr
    ) where

import Control.Applicative
import Control.Concurrent.Async (Async, Concurrently(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Managed (MonadManaged(..))
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Streaming.Zlib (Inflate, Popper, PopperRes(..), WindowBits(..))
import Data.Text (Text)
import Data.Text.Encoding (Decoding(..))
import System.Exit (ExitCode(..))
import System.IO (Handle)
import Turtle.Internal (ignoreSIGPIPE)
import Turtle.Prelude (ProcFailed(..), ShellFailed(..))
import Turtle.Shell (Shell(..), FoldShell(..), fold, sh)

import qualified Control.Concurrent.Async      as Async
import qualified Control.Concurrent.STM        as STM
import qualified Control.Concurrent.MVar       as MVar
import qualified Control.Concurrent.STM.TQueue as TQueue
import qualified Control.Exception             as Exception
import qualified Control.Foldl
import qualified Control.Monad
import qualified Control.Monad.Managed         as Managed
import qualified Data.ByteString
import qualified Data.Streaming.Zlib           as Zlib
import qualified Data.Text
import qualified Data.Text.Encoding            as Encoding
import qualified Data.Text.Encoding.Error      as Encoding.Error
import qualified Foreign
import qualified System.IO
import qualified System.Process                as Process
import qualified Turtle.Prelude

{-| Read chunks of bytes from standard input

    The chunks are not necessarily aligned to line boundaries
-}
stdin :: Shell ByteString
stdin = inhandle System.IO.stdin

{-| Read chunks of bytes from a file

    The chunks are not necessarily aligned to line boundaries
-}
input :: FilePath -> Shell ByteString
input file = do
    handle <- using (Turtle.Prelude.readonly file)
    inhandle handle

{-| Read chunks of bytes from a `Handle`

    The chunks are not necessarily aligned to line boundaries
-}
inhandle :: Handle -> Shell ByteString
inhandle handle = Shell (\(FoldShell step begin done) -> do
    let loop x = do
            eof <- System.IO.hIsEOF handle
            if eof
                then done x
                else do
                    bytes <- Data.ByteString.hGetSome handle defaultChunkSize
                    x'    <- step x bytes
                    loop $! x'
    loop $! begin )
  where
    -- Copied from `Data.ByteString.Lazy.Internal`
    defaultChunkSize :: Int
    defaultChunkSize = 32 * 1024 - 2 * Foreign.sizeOf (undefined :: Int)

{-| Stream chunks of bytes to standard output

    The chunks are not necessarily aligned to line boundaries
-}
stdout :: MonadIO io => Shell ByteString -> io ()
stdout s = sh (do
    bytes <- s
    liftIO (Data.ByteString.hPut System.IO.stdout bytes) )

{-| Stream chunks of bytes to a file

    The chunks do not need to be aligned to line boundaries
-}
output :: MonadIO io => FilePath -> Shell ByteString -> io ()
output file s = sh (do
    handle <- using (Turtle.Prelude.writeonly file)
    bytes  <- s
    liftIO (Data.ByteString.hPut handle bytes) )

{-| Stream chunks of bytes to a `Handle`

    The chunks do not need to be aligned to line boundaries
-}
outhandle :: MonadIO io => Handle -> Shell ByteString -> io ()
outhandle handle s = sh (do
    bytes <- s
    liftIO (Data.ByteString.hPut handle bytes) )

{-| Append chunks of bytes to append to a file

    The chunks do not need to be aligned to line boundaries
-}
append :: MonadIO io => FilePath -> Shell ByteString -> io ()
append file s = sh (do
    handle <- using (Turtle.Prelude.appendonly file)
    bytes  <- s
    liftIO (Data.ByteString.hPut handle bytes) )

{-| Stream chunks of bytes to standard error

    The chunks do not need to be aligned to line boundaries
-}
stderr :: MonadIO io => Shell ByteString -> io ()
stderr s = sh (do
    bytes <- s
    liftIO (Data.ByteString.hPut System.IO.stderr bytes) )

-- | Read in a stream's contents strictly
strict :: MonadIO io => Shell ByteString -> io ByteString
strict s = do
    listOfByteStrings <- fold s Control.Foldl.list
    return (Data.ByteString.concat listOfByteStrings)

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
    non-lazy blob of Text

    The command inherits @stderr@ for the current process
-}
procStrict
    :: MonadIO io
    => Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io (ExitCode, ByteString)
    -- ^ Exit code and stdout
procStrict cmd args =
    systemStrict (Process.proc (Data.Text.unpack cmd) (map Data.Text.unpack args))

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
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io (ExitCode, ByteString)
    -- ^ Exit code and stdout
shellStrict cmdline = systemStrict (Process.shell (Data.Text.unpack cmdline))

{-| Run a command using @execvp@, retrieving the exit code, stdout, and stderr
    as a non-lazy blob of Text
-}
procStrictWithErr
    :: MonadIO io
    => Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io (ExitCode, ByteString, ByteString)
    -- ^ (Exit code, stdout, stderr)
procStrictWithErr cmd args =
    systemStrictWithErr (Process.proc (Data.Text.unpack cmd) (map Data.Text.unpack args))

{-| Run a command line using the shell, retrieving the exit code, stdout, and
    stderr as a non-lazy blob of Text

    This command is more powerful than `proc`, but highly vulnerable to code
    injection if you template the command line with untrusted input
-}
shellStrictWithErr
    :: MonadIO io
    => Text
    -- ^ Command line
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io (ExitCode, ByteString, ByteString)
    -- ^ (Exit code, stdout, stderr)
shellStrictWithErr cmdline =
    systemStrictWithErr (Process.shell (Data.Text.unpack cmdline))

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

{-| `systemStrict` generalizes `shellStrict` and `procStrict` by allowing you to
    supply your own custom `CreateProcess`.  This is for advanced users who feel
    comfortable using the lower-level @process@ API
-}
systemStrict
    :: MonadIO io
    => Process.CreateProcess
    -- ^ Command
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io (ExitCode, ByteString)
    -- ^ Exit code and stdout
systemStrict p s = liftIO (do
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
            (Data.ByteString.hGetContents hOut) ) )

{-| `systemStrictWithErr` generalizes `shellStrictWithErr` and
    `procStrictWithErr` by allowing you to supply your own custom
    `CreateProcess`.  This is for advanced users who feel comfortable using
    the lower-level @process@ API
-}
systemStrictWithErr
    :: MonadIO io
    => Process.CreateProcess
    -- ^ Command
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> io (ExitCode, ByteString, ByteString)
    -- ^ Exit code and stdout
systemStrictWithErr p s = liftIO (do
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
            <*> Concurrently (Data.ByteString.hGetContents hOut)
            <*> Concurrently (Data.ByteString.hGetContents hErr) ) )

{-| Run a command using @execvp@, streaming @stdout@ as chunks of `ByteString`

    The command inherits @stderr@ for the current process
-}
inproc
    :: Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> Shell ByteString
    -- ^ Chunks of bytes read from process output
inproc cmd args =
    stream (Process.proc (Data.Text.unpack cmd) (map Data.Text.unpack args))

{-| Run a command line using the shell, streaming @stdout@ as chunks of
    `ByteString`

    This command is more powerful than `inproc`, but highly vulnerable to code
    injection if you template the command line with untrusted input

    The command inherits @stderr@ for the current process
-}
inshell
    :: Text
    -- ^ Command line
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> Shell ByteString
    -- ^ Chunks of bytes read from process output
inshell cmd = stream (Process.shell (Data.Text.unpack cmd))

waitForProcessThrows :: Process.ProcessHandle -> IO ()
waitForProcessThrows ph = do
    exitCode <- Process.waitForProcess ph
    case exitCode of
        ExitSuccess   -> return ()
        ExitFailure _ -> Exception.throwIO exitCode

{-| `stream` generalizes `inproc` and `inshell` by allowing you to supply your
    own custom `CreateProcess`.  This is for advanced users who feel comfortable
    using the lower-level @process@ API

    Throws an `ExitCode` exception if the command returns a non-zero exit code
-}
stream
    :: Process.CreateProcess
    -- ^ Command
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> Shell ByteString
    -- ^ Chunks of bytes read from process output
stream p s = do
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
    mvar <- liftIO (MVar.newMVar False)
    let close handle = do
            MVar.modifyMVar_ mvar (\finalized -> do
                Control.Monad.unless finalized (ignoreSIGPIPE (System.IO.hClose handle))
                return True )

    (hIn, hOut, ph) <- using (Managed.managed (Exception.bracket open (\(hIn, _, ph) -> close hIn >> Process.terminateProcess ph)))
    let feedIn :: (forall a. IO a -> IO a) -> IO ()
        feedIn restore =
            restore (ignoreSIGPIPE (sh (do
                bytes <- s
                liftIO (Data.ByteString.hPut hIn bytes) ) ) )
            `Exception.finally` close hIn

    a <- using
        (Managed.managed (\k ->
            Exception.mask (\restore ->
                Async.withAsync (feedIn restore) k ) ))
    inhandle hOut <|> (liftIO (waitForProcessThrows ph *> halt a) *> empty)

{-| `streamWithErr` generalizes `inprocWithErr` and `inshellWithErr` by allowing
    you to supply your own custom `CreateProcess`.  This is for advanced users
    who feel comfortable using the lower-level @process@ API

    Throws an `ExitCode` exception if the command returns a non-zero exit code
-}
streamWithErr
    :: Process.CreateProcess
    -- ^ Command
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> Shell (Either ByteString ByteString)
    -- ^ Chunks of bytes read from process output
streamWithErr p s = do
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
    mvar <- liftIO (MVar.newMVar False)
    let close handle = do
            MVar.modifyMVar_ mvar (\finalized -> do
                Control.Monad.unless finalized (ignoreSIGPIPE (System.IO.hClose handle))
                return True )

    (hIn, hOut, hErr, ph) <- using (Managed.managed (Exception.bracket open (\(hIn, _, _, ph) -> close hIn >> Process.terminateProcess ph)))
    let feedIn :: (forall a. IO a -> IO a) -> IO ()
        feedIn restore =
            restore (ignoreSIGPIPE (sh (do
                bytes <- s
                liftIO (Data.ByteString.hPut hIn bytes) ) ) )
            `Exception.finally` close hIn

    queue <- liftIO TQueue.newTQueueIO
    let forwardOut :: (forall a. IO a -> IO a) -> IO ()
        forwardOut restore =
            restore (sh (do
                bytes <- inhandle hOut
                liftIO (STM.atomically (TQueue.writeTQueue queue (Just (Right bytes)))) ))
            `Exception.finally` STM.atomically (TQueue.writeTQueue queue Nothing)
    let forwardErr :: (forall a. IO a -> IO a) -> IO ()
        forwardErr restore =
            restore (sh (do
                bytes <- inhandle hErr
                liftIO (STM.atomically (TQueue.writeTQueue queue (Just (Left  bytes)))) ))
            `Exception.finally` STM.atomically (TQueue.writeTQueue queue Nothing)
    let drain = Shell (\(FoldShell step begin done) -> do
            let loop x numNothing
                    | numNothing < 2 = do
                        m <- STM.atomically (TQueue.readTQueue queue)
                        case m of
                            Nothing -> loop x $! numNothing + 1
                            Just e  -> do
                                x' <- step x e
                                loop x' numNothing
                    | otherwise      = return x
            x1 <- loop begin (0 :: Int)
            done x1 )

    a <- using
        (Managed.managed (\k ->
            Exception.mask (\restore ->
                Async.withAsync (feedIn restore) k ) ))
    b <- using
        (Managed.managed (\k ->
            Exception.mask (\restore ->
                Async.withAsync (forwardOut restore) k ) ))
    c <- using
        (Managed.managed (\k ->
            Exception.mask (\restore ->
                Async.withAsync (forwardErr restore) k ) ))
    let l `also` r = do
            _ <- l <|> (r *> STM.retry)
            _ <- r
            return ()
    let waitAll = STM.atomically (Async.waitSTM a `also` (Async.waitSTM b `also` Async.waitSTM c))
    drain <|> (liftIO (waitForProcessThrows ph *> waitAll) *> empty)

{-| Run a command using the shell, streaming @stdout@ and @stderr@ as chunks of
    `ByteString`.  Chunks from @stdout@ are wrapped in `Right` and chunks from
    @stderr@ are wrapped in `Left`.

    Throws an `ExitCode` exception if the command returns a non-zero exit code
-}
inprocWithErr
    :: Text
    -- ^ Command
    -> [Text]
    -- ^ Arguments
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> Shell (Either ByteString ByteString)
    -- ^ Chunks of either output (`Right`) or error (`Left`)
inprocWithErr cmd args =
    streamWithErr (Process.proc (Data.Text.unpack cmd) (map Data.Text.unpack args))


{-| Run a command line using the shell, streaming @stdout@ and @stderr@ as
    chunks of `ByteString`.  Chunks from @stdout@ are wrapped in `Right` and
    chunks from @stderr@ are wrapped in `Left`.

    This command is more powerful than `inprocWithErr`, but highly vulnerable to
    code injection if you template the command line with untrusted input

    Throws an `ExitCode` exception if the command returns a non-zero exit code
-}
inshellWithErr
    :: Text
    -- ^ Command line
    -> Shell ByteString
    -- ^ Chunks of bytes written to process input
    -> Shell (Either ByteString ByteString)
    -- ^ Chunks of either output (`Right`) or error (`Left`)
inshellWithErr cmd = streamWithErr (Process.shell (Data.Text.unpack cmd))

-- | Internal utility used by both `compress` and `decompress`
fromPopper :: Popper -> Shell ByteString
fromPopper popper = loop
  where
    loop = do
        result <- liftIO popper

        case result of
            PRDone ->
                empty
            PRNext compressedByteString ->
                return compressedByteString <|> loop
            PRError exception ->
                liftIO (Exception.throwIO exception)

{-| Compress a stream using @zlib@

    Note that this can decompress streams that are the concatenation of
    multiple compressed streams (just like @gzip@)

>>> let compressed = select [ "ABC", "DEF" ] & compress 0 defaultWindowBits
>>> compressed & decompress defaultWindowBits & view
"ABCDEF"
>>> (compressed <|> compressed) & decompress defaultWindowBits & view
"ABCDEF"
"ABCDEF"
-}
compress
    :: Int
    -- ^ Compression level
    -> WindowBits
    -- ^
    -> Shell ByteString
    -- ^
    -> Shell ByteString
compress compressionLevel windowBits bytestrings = do
    deflate <- liftIO (Zlib.initDeflate compressionLevel windowBits)

    let loop = do
            bytestring <- bytestrings

            popper <- liftIO (Zlib.feedDeflate deflate bytestring)

            fromPopper popper

    let wrapUp = do
            let popper = liftIO (Zlib.finishDeflate deflate)

            fromPopper popper

    loop <|> wrapUp

data DecompressionState = Uninitialized | Decompressing Inflate

-- | Decompress a stream using @zlib@ (just like the @gzip@ command)
decompress :: WindowBits -> Shell ByteString -> Shell ByteString
decompress windowBits (Shell k) = Shell k'
  where
    k' (FoldShell step begin done) = k (FoldShell step' begin' done')
      where
        begin' = (begin, Uninitialized)

        step' (x0, Uninitialized) compressedByteString = do
            inflate <- Zlib.initInflate windowBits

            step' (x0, Decompressing inflate) compressedByteString
        step' (x0, Decompressing inflate) compressedByteString = do
            popper <- Zlib.feedInflate inflate compressedByteString

            let loop x = do
                    result <- popper

                    case result of
                        PRDone -> do
                            compressedByteString' <- Zlib.getUnusedInflate inflate

                            if Data.ByteString.null compressedByteString'
                                then return (x, Decompressing inflate)
                                else do
                                    decompressedByteString <- Zlib.finishInflate inflate

                                    x' <- step x decompressedByteString

                                    step' (x', Uninitialized) compressedByteString'
                        PRNext decompressedByteString -> do
                            x' <- step x decompressedByteString

                            loop x'
                        PRError exception -> do
                            Exception.throwIO exception

            loop x0

        done' (x0, Uninitialized) = do
            done x0
        done' (x0, Decompressing inflate) = do
            decompressedByteString <- Zlib.finishInflate inflate

            x0' <- step x0 decompressedByteString

            done' (x0', Uninitialized)

{-| Decode a stream of bytes as UTF8 `Text`

    NOTE: This function will throw a pure exception (i.e. an `error`) if UTF8
    decoding fails (mainly due to limitations in the @text@ package's stream
    decoding API)
-}
toUTF8 :: Shell ByteString -> Shell Text
toUTF8 (Shell k) = Shell k'
  where
    k' (FoldShell step begin done) =
        k (FoldShell step' begin' done')
      where
        begin' =
            (mempty, Encoding.streamDecodeUtf8With Encoding.Error.strictDecode, begin)

        step' (prefix, decoder, x) suffix = do
            let bytes = prefix <> suffix

            let Some text prefix' decoder' = decoder bytes 

            x' <- step x text

            return (prefix', decoder', x')

        done' (_, _, x) = do
            done x

-- | Encode a stream of bytes as UTF8 `Text`
fromUTF8 :: Shell Text -> Shell ByteString
fromUTF8 = fmap Encoding.encodeUtf8
