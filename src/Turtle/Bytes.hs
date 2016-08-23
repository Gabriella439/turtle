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
    ) where

import Control.Foldl (FoldM(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Managed (MonadManaged(..))
import Data.ByteString (ByteString)
import Filesystem.Path (FilePath)
import Prelude hiding (FilePath)
import System.IO (Handle)
import Turtle.Prelude (appendonly, readonly, writeonly)
import Turtle.Shell (Shell(..), fold, sh)

import qualified Control.Foldl
import qualified Data.ByteString
import qualified Foreign
import qualified System.IO

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
    handle <- using (readonly file)
    inhandle handle

{-| Read chunks of bytes from a `Handle`

    The chunks are not necessarily aligned to line boundaries
-}
inhandle :: Handle -> Shell ByteString
inhandle handle = Shell (\(FoldM step begin done) -> do
    x0 <- begin
    let loop x = do
            eof <- System.IO.hIsEOF handle
            if eof
                then done x
                else do
                    bytes <- Data.ByteString.hGetSome handle defaultChunkSize
                    x'    <- step x bytes
                    loop $! x'
    loop $! x0 )
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
    handle <- using (writeonly file)
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
    handle <- using (appendonly file)
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
strict s = fmap Data.ByteString.concat (fold s Control.Foldl.list)
