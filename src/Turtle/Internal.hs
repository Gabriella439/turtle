module Turtle.Internal where

import Control.Exception (handle, throwIO)
import Data.Text (Text)
import Foreign.C.Error (Errno(..), ePIPE)
import GHC.IO.Exception (IOErrorType(..), IOException(..))

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.FilePath as FilePath

ignoreSIGPIPE :: IO () -> IO ()
ignoreSIGPIPE = handle (\e -> case e of
    IOError
        { ioe_type = ResourceVanished
        , ioe_errno = Just ioe }
        | Errno ioe == ePIPE -> return ()
    _ -> throwIO e
    )

{-| Convert a `FilePath` to human-readable `Text`

    Note that even though the type says `Either` this utility actually always
    succeeds and returns a `Right` value.  The only reason for the `Either` is
    compatibility with the old type from the @system-filepath@ package.
-}
toText :: FilePath -> Either Text Text
toText = Right . Text.pack
{-# DEPRECATED toText "Use Data.Text.pack instead" #-}

-- | Convert `Text` to a `FilePath`
fromText :: Text -> FilePath
fromText = Text.unpack
{-# DEPRECATED fromText "Use Data.Text.unpack instead" #-}

-- | Convert a `String` to a `FilePath`
decodeString :: String -> FilePath
decodeString = id
{-# DEPRECATED decodeString "Use id instead" #-}

-- | Convert a `FilePath` to a `String`
encodeString :: FilePath -> String
encodeString = id
{-# DEPRECATED encodeString "Use id instead" #-}

-- | Remove a prefix from a path
stripPrefix :: FilePath -> FilePath -> Maybe FilePath
stripPrefix prefix path = do
    suffix <- List.stripPrefix prefixComponents pathComponents

    return (FilePath.joinPath suffix)
  where
    prefixComponents = FilePath.splitPath prefix

    pathComponents = FilePath.splitPath path

-- | Read in a file as `Text`
readTextFile :: FilePath -> IO Text
readTextFile = Text.IO.readFile
{-# DEPRECATED readTextFile "Use Data.Text.IO.readFile instead" #-}

-- | Write out a file as `Text`
writeTextFile :: FilePath -> Text -> IO ()
writeTextFile = Text.IO.writeFile
{-# DEPRECATED writeTextFile "Use Data.Text.IO.writeFile instead" #-}

-- | Retrieves the `FilePath`'s root
root :: FilePath -> FilePath
root = fst . FilePath.splitDrive

-- | Retrieves the `FilePath`'s directory
directory :: FilePath -> FilePath
directory = FilePath.takeDirectory
{-# DEPRECATED directory "Use System.FilePath.takeDirectory instead" #-}

-- | Retrieves the `FilePath`'s filename component
filename :: FilePath -> FilePath
filename = FilePath.takeFileName
{-# DEPRECATED filename "Use System.FilePath.takeFileName instead" #-}

-- | Retrieve a `FilePath`'s directory name
dirname :: FilePath -> FilePath
dirname path = FilePath.joinPath (loop (FilePath.splitPath path))
  where
    loop [ x, _ ] = [ x ]
    loop [ _ ]    = [ ]
    loop [ ]      = [ ]
    loop (_ : xs) = loop xs

-- | Retrieve a `FilePath`'s basename component
basename :: FilePath -> String
basename = FilePath.takeBaseName
{-# DEPRECATED basename "Use System.FilePath.takeBaseName instead" #-}

-- | Test whether a path is absolute
absolute :: FilePath -> Bool
absolute = FilePath.isAbsolute
{-# DEPRECATED absolute "Use System.FilePath.isAbsolute instead" #-}

-- | Test whether a path is relative
relative :: FilePath -> Bool
relative = FilePath.isRelative
{-# DEPRECATED relative "Use System.FilePath.isRelative instead" #-}

-- | Split a `FilePath` into its components
splitDirectories :: FilePath -> [FilePath]
splitDirectories = FilePath.splitPath
{-# DEPRECATED splitDirectories "Use System.FilePath.splitPath instead" #-}

-- | Get a `FilePath`'s last extension, or `Nothing` if it has no extension
extension :: FilePath -> Maybe String
extension path
    | suffix == "" = Nothing
    | otherwise    = Just suffix
  where
    suffix = FilePath.takeExtension path
{-# DEPRECATED extension "Use System.FilePath.takeExtension instead" #-}

-- | Split a `FilePath` on its extension
splitExtension :: FilePath -> (String, Maybe String)
splitExtension path
    | suffix == "" = (prefix, Nothing)
    | otherwise    = (prefix, Just suffix)
  where
    (prefix, suffix) = FilePath.splitExtension path
{-# DEPRECATED splitExtension "Use System.FilePath.splitExtension instead" #-}
