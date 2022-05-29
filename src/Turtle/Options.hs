{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Example usage of this module:
--
-- > -- options.hs
-- >
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Turtle
-- >
-- > parser :: Parser (Text, Int)
-- > parser = (,) <$> optText "name" 'n' "Your first name"
-- >              <*> optInt  "age"  'a' "Your current age"
-- >
-- > main = do
-- >     (name, age) <- options "Greeting script" parser
-- >     echo (repr (format ("Hello there, "%s) name))
-- >     echo (repr (format ("You are "%d%" years old") age))
--
-- > $ ./options --name John --age 42
-- > Hello there, John
-- > You are 42 years old
--
-- > $ ./options --help
-- > Greeting script
-- >
-- > Usage: options (-n|--name NAME) (-a|--age AGE)
-- >
-- > Available options:
-- >  -h,--help                Show this help text
-- >  --name NAME              Your first name
-- >  --age AGE                Your current age
--
-- See the "Turtle.Tutorial" module which contains more examples on how to use
-- command-line parsing.

module Turtle.Options
    ( -- * Types
      Parser
    , ArgName(..)
    , CommandName(..)
    , ShortName
    , Description(..)
    , HelpMessage(..)

      -- * Flag-based option parsers
    , switch
    , optText
    , optLine
    , optInt
    , optInteger
    , optDouble
    , optPath
    , optRead
    , opt

    -- * Positional argument parsers
    , argText
    , argLine
    , argInt
    , argInteger
    , argDouble
    , argPath
    , argRead
    , arg

      -- * Consume parsers
    , subcommand
    , subcommandGroup
    , options
    , optionsExt

    ) where

import Data.Monoid
import Data.Foldable
import Data.String (IsString)
import Text.Read (readMaybe)
import Data.Text (Text)
import Data.Optional
import Control.Applicative
import Control.Monad.IO.Class
import Options.Applicative (Parser)
import Text.PrettyPrint.ANSI.Leijen (Doc, displayS, renderCompact)
import Turtle.Line (Line)

import qualified Data.Text as Text
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Types as Opts
import qualified Turtle.Line

-- | Parse the given options from the command line
options :: MonadIO io => Description -> Parser a -> io a
options desc parser = liftIO
    $ Opts.customExecParser (Opts.prefs prefs)
    $ Opts.info (Opts.helper <*> parser)
                (Opts.headerDoc (Just (getDescription desc)))
  where
    prefs :: Opts.PrefsMod
#if MIN_VERSION_optparse_applicative(0,13,0)
    prefs = Opts.showHelpOnError <> Opts.showHelpOnEmpty
#else
    prefs = Opts.showHelpOnError
#endif

{-| Parse the given options from the command line and add additional information

    Extended version of @options@ with program version header and footer information
-}
optionsExt :: MonadIO io => Header -> Footer  -> Description ->  Version -> Parser a -> io a
optionsExt header footer desc version parser = liftIO
    $ Opts.customExecParser (Opts.prefs prefs)
    $ Opts.info (Opts.helper <*> versionOption <*> parser)
                (Opts.headerDoc (Just (getHeader header)) <>
                 Opts.footerDoc (Just (getFooter footer)) <>
                 Opts.progDescDoc (Just (getDescription desc)))
  where
    versionOption =
      Opts.infoOption
        (Text.unpack version)
        (Opts.long "version" <> Opts.help "Show version")
    prefs :: Opts.PrefsMod
#if MIN_VERSION_optparse_applicative(0,13,0)
    prefs = Opts.showHelpOnError <> Opts.showHelpOnEmpty
#else
    prefs = Opts.showHelpOnError
#endif


{-| The name of a command-line argument

    This is used to infer the long name and metavariable for the command line
    flag.  For example, an `ArgName` of @\"name\"@ will create a @--name@ flag
    with a @NAME@ metavariable
-}
newtype ArgName = ArgName { getArgName :: Text }
    deriving (IsString)

-- | The short one-character abbreviation for a flag (i.e. @-n@)
type ShortName = Char

{-| The name of a sub-command

    This is lower-cased to create a sub-command.  For example, a `CommandName` of
    @\"Name\"@ will parse `name` on the command line before parsing the
    remaining arguments using the command's subparser.
-}
newtype CommandName = CommandName { getCommandName :: Text }
    deriving (IsString)

{-| A brief description of what your program does

    This description will appear in the header of the @--help@ output
-}
newtype Description = Description { getDescription :: Doc }
    deriving (IsString)

{-| Header of the program

    This description will appear in the header of the @--help@ output
-}
newtype Header = Header { getHeader :: Doc }
    deriving (IsString)
{-| Footer of the program

    This description will appear in the footer of the @--help@ output
-}
newtype Footer = Fotter { getFooter :: Doc }
    deriving (IsString)

-- | Program Version
type Version = Text
{-| A helpful message explaining what a flag does

    This will appear in the @--help@ output
-}
newtype HelpMessage = HelpMessage { getHelpMessage :: Text }
    deriving (IsString)

{-| This parser returns `True` if the given flag is set and `False` if the
    flag is absent
-}
switch
    :: ArgName
    -> ShortName
    -> Optional HelpMessage
    -> Parser Bool
switch argName c helpMessage
   = Opts.switch
   $ (Opts.long . Text.unpack . getArgName) argName
  <> Opts.short c
  <> foldMap (Opts.help . Text.unpack . getHelpMessage) helpMessage

{- | Build a flag-based option parser for any type by providing a `Text`-parsing
     function
-}
opt :: (Text -> Maybe a)
    -> ArgName
    -> ShortName
    -> Optional HelpMessage
    -> Parser a
opt argParse argName c helpMessage
   = Opts.option (argParseToReadM argParse)
   $ Opts.metavar (Text.unpack (Text.toUpper (getArgName argName)))
  <> Opts.long (Text.unpack (getArgName argName))
  <> Opts.short c
  <> foldMap (Opts.help . Text.unpack . getHelpMessage) helpMessage

-- | Parse any type that implements `Read`
optRead :: Read a => ArgName -> ShortName -> Optional HelpMessage -> Parser a
optRead = opt (readMaybe . Text.unpack)

-- | Parse an `Int` as a flag-based option
optInt :: ArgName -> ShortName -> Optional HelpMessage -> Parser Int
optInt = optRead

-- | Parse an `Integer` as a flag-based option
optInteger :: ArgName -> ShortName -> Optional HelpMessage -> Parser Integer
optInteger = optRead

-- | Parse a `Double` as a flag-based option
optDouble :: ArgName -> ShortName -> Optional HelpMessage -> Parser Double
optDouble = optRead

-- | Parse a `Text` value as a flag-based option
optText :: ArgName -> ShortName -> Optional HelpMessage -> Parser Text
optText = opt Just

-- | Parse a `Line` value as a flag-based option
optLine :: ArgName -> ShortName -> Optional HelpMessage -> Parser Line
optLine = opt Turtle.Line.textToLine

-- | Parse a `FilePath` value as a flag-based option
optPath :: ArgName -> ShortName -> Optional HelpMessage -> Parser FilePath
optPath argName short msg = fmap Text.unpack (optText argName short msg)

{- | Build a positional argument parser for any type by providing a
    `Text`-parsing function
-}
arg :: (Text -> Maybe a)
    -> ArgName
    -> Optional HelpMessage
    -> Parser a
arg argParse argName helpMessage
   = Opts.argument (argParseToReadM argParse)
   $ Opts.metavar (Text.unpack (Text.toUpper (getArgName argName)))
  <> foldMap (Opts.help . Text.unpack . getHelpMessage) helpMessage

-- | Parse any type that implements `Read` as a positional argument
argRead :: Read a => ArgName -> Optional HelpMessage -> Parser a
argRead = arg (readMaybe . Text.unpack)

-- | Parse an `Int` as a positional argument
argInt :: ArgName -> Optional HelpMessage -> Parser Int
argInt = argRead

-- | Parse an `Integer` as a positional argument
argInteger :: ArgName -> Optional HelpMessage -> Parser Integer
argInteger = argRead

-- | Parse a `Double` as a positional argument
argDouble :: ArgName -> Optional HelpMessage -> Parser Double
argDouble = argRead

-- | Parse a `Text` as a positional argument
argText :: ArgName -> Optional HelpMessage -> Parser Text
argText = arg Just

-- | Parse a `Line` as a positional argument
argLine :: ArgName -> Optional HelpMessage -> Parser Line
argLine = arg Turtle.Line.textToLine

-- | Parse a `FilePath` as a positional argument
argPath :: ArgName -> Optional HelpMessage -> Parser FilePath
argPath argName msg = fmap Text.unpack (argText argName msg)

argParseToReadM :: (Text -> Maybe a) -> Opts.ReadM a
argParseToReadM f = do
    s <- Opts.readerAsk
    case f (Text.pack s) of
        Just a -> return a
        Nothing -> Opts.readerAbort (Opts.ShowHelpText Nothing)

{-| Create a sub-command that parses `CommandName` and then parses the rest
    of the command-line arguments

    The sub-command will have its own `Description` and help text
-}
subcommand :: CommandName -> Description -> Parser a -> Parser a
subcommand cmdName desc p =
    Opts.hsubparser (Opts.command name info <> Opts.metavar name)
  where
    name = Text.unpack (getCommandName cmdName)

    info = Opts.info p (Opts.progDescDoc (Just (getDescription desc)))

-- | Create a named group of sub-commands
subcommandGroup :: forall a. Description -> [(CommandName, Description, Parser a)] -> Parser a
subcommandGroup name cmds =
    Opts.hsubparser (Opts.commandGroup name' <> foldMap f cmds <> Opts.metavar metavar)
  where
    f :: (CommandName, Description, Parser a) -> Opts.Mod Opts.CommandFields a
    f (cmdName, desc, p) =
        Opts.command
            (Text.unpack (getCommandName cmdName))
            (Opts.info p (Opts.progDescDoc (Just (getDescription desc))))

    metavar :: String
    metavar = Text.unpack (Text.intercalate " | " (map g cmds))
      where
        g :: (CommandName, Description, Parser a) -> Text
        g (cmdName, _, _) = getCommandName cmdName

    name' :: String
    name' = displayS (renderCompact (getDescription name)) ""
