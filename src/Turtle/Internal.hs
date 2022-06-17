module Turtle.Internal where

import Control.Applicative ((<|>))
import Control.Exception (handle, throwIO)
import Data.Text (Text)
import Foreign.C.Error (Errno(..), ePIPE)
import GHC.IO.Exception (IOErrorType(..), IOException(..))
import System.FilePath ((</>))

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

-- | Find the greatest common prefix between a list of `FilePath`s
commonPrefix :: [FilePath] -> FilePath
commonPrefix [ ] = mempty
commonPrefix (path : paths) = foldr longestPathPrefix path paths
  where
    longestPathPrefix left right
        | leftComponents == rightComponents =
               FilePath.joinPath leftComponents
            <> mconcat (longestPrefix leftExtensions rightExtensions)
        | otherwise =
           FilePath.joinPath (longestPrefix leftComponents rightComponents)
      where
        (leftComponents, leftExtensions)  = splitExt (splitDirectories left)

        (rightComponents, rightExtensions) = splitExt (splitDirectories right)

longestPrefix :: Eq a => [a] -> [a] -> [a]
longestPrefix (l : ls) (r : rs)
    | l == r = l : longestPrefix ls rs
longestPrefix _ _ = [ ]

-- | Remove a prefix from a path
stripPrefix :: FilePath -> FilePath -> Maybe FilePath
stripPrefix prefix path = do
    componentSuffix <- List.stripPrefix prefixComponents pathComponents

    if null componentSuffix
        then do
            prefixSuffix <- List.stripPrefix prefixExtensions pathExtensions

            return (mconcat prefixSuffix)
        else do
            return (FilePath.joinPath componentSuffix <> mconcat pathExtensions)
  where
    (prefixComponents, prefixExtensions) = splitExt (splitDirectories prefix)

    (pathComponents, pathExtensions) = splitExt (splitDirectories path)

-- Internal helper function for `stripPrefix` and `commonPrefix`
splitExt :: [FilePath] -> ([FilePath], [String])
splitExt [ component ] = ([ base ], map ("." ++) exts)
  where
    (base, exts) = splitExtensions component
splitExt [ ] =
    ([ ], [ ])
splitExt (component : components) = (component : base, exts)
  where
    (base, exts) = splitExt components

-- | Normalise a path
collapse :: FilePath -> FilePath
collapse = FilePath.normalise
{-# DEPRECATED collapse "Use System.FilePath.normalise instead" #-}

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

-- | Retrieves the `FilePath`'s parent directory
parent :: FilePath -> FilePath
parent path = prefix </> suffix
  where
    (drive, rest) = FilePath.splitDrive path

    components = loop (splitDirectories rest)

    prefix =
        case components of
            "./"  : _ -> drive
            "../" : _ -> drive
            _ | null drive -> "./"
              | otherwise  -> drive

    suffix = FilePath.joinPath components

    loop [ _ ]    = [ ]
    loop [ ]      = [ ]
    loop (c : cs) = c : loop cs

-- | Retrieves the `FilePath`'s directory
directory :: FilePath -> FilePath
directory path
    | prefix == "" && suffix == ".." =
        "../"
    | otherwise =
        trailingSlash (FilePath.takeDirectory prefix) ++ suffix
  where
    (prefix, suffix) = trailingParent path
      where
        trailingParent ".."     = (""      , "..")
        trailingParent [ a, b ] = ([ a, b ], ""  )
        trailingParent [ a ]    = ([ a ]   , ""  )
        trailingParent [ ]      = ([ ]     , ""  )
        trailingParent (c : cs) = (c : p, s)
          where
            ~(p, s) = trailingParent cs

    trailingSlash ""       = "/"
    trailingSlash "/"      = "/"
    trailingSlash (c : cs) = c : trailingSlash cs

-- | Retrieves the `FilePath`'s filename component
filename :: FilePath -> FilePath
filename path
    | result == "." || result == ".." = ""
    | otherwise                       = result
  where
    result = FilePath.takeFileName path

-- | Retrieve a `FilePath`'s directory name
dirname :: FilePath -> FilePath
dirname path = loop (splitDirectories path)
  where
    loop [ x, y ] =
        case deslash y <|> deslash x of
            Just name -> name
            Nothing   -> ""
    loop [ x ] =
        case deslash x of
            Just name -> name
            Nothing   -> ""
    loop [ ] =
        ""
    loop (_ : xs) =
        loop xs

    deslash ""       = Nothing
    deslash "/"      = Just ""
    deslash (c : cs) = fmap (c :) (deslash cs)

-- | Retrieve a `FilePath`'s basename component
basename :: FilePath -> String
basename path =
    case name of
        '.' : _ -> name
        _ ->
            case splitExtensions name of
                (base, _) -> base
  where
    name = filename path

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
splitDirectories path = loop (FilePath.splitPath path)
  where
    loop [ ]      = [ ]
    loop [ ".." ] = [ "../" ]
    loop [ "." ]  = [ "./" ]
    loop (c : cs) = c : loop cs

-- | Get a `FilePath`'s last extension, or `Nothing` if it has no extension
extension :: FilePath -> Maybe String
extension path =
    case suffix of
        '.' : ext -> Just ext
        _         -> Nothing
  where
    suffix = FilePath.takeExtension path

-- | Split a `FilePath` on its extension
splitExtension :: FilePath -> (String, Maybe String)
splitExtension path =
    case suffix of
        '.' : ext -> (prefix, Just ext)
        _         -> (prefix, Nothing)
  where
    (prefix, suffix) = FilePath.splitExtension path

-- | Split a `FilePath` on its extensions
splitExtensions :: FilePath -> (String, [String])
splitExtensions path0 = (prefix0, reverse exts0)
  where
    (prefix0, exts0) = loop path0

    loop path = case splitExtension path of
        (prefix, Just ext) ->
            (base, ext : exts)
          where
            (base, exts) = loop prefix
        (base, Nothing) ->
            (base, [])
