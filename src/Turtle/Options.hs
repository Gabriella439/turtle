{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Example usage of this module:
--
-- > -- options.hs
-- >
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Turtle
-- >
-- > parser :: Parser (Text, Int)
-- > parser = (,) <$> argText     "name" "Your first name"
-- >              <*> argIntegral "age"  "Your current age"
-- >
-- > main = do
-- >     (name, age) <- options "Greeting script" parser
-- >     echo (format ("Hello there, "%s) name)
-- >     echo (format ("You are "%d%" years old") age)
--
-- > $ ./options --name John --age 42
-- > Hello there, John
-- > You are 42 years old
--
-- > $ ./options --help
-- > Greeting script
-- >
-- > Usage: options --name NAME --age AGE
-- >
-- > Available options:
-- >  -h,--help                Show this help text
-- >  --name NAME              Your first name
-- >  --age AGE                Your current age

module Turtle.Options
    ( -- * Types
      Parser
    , ArgName
    , Description
    , HelpMessage

      -- * Build parsers
    , switch
    , argText
    , argIntegral
    , argFractional
    , argRead
    , arg

      -- * Consume parsers
    , options

    ) where

import Data.Monoid
import Data.Foldable
import Data.String (IsString)
import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Optional
import Control.Applicative
import Control.Monad.IO.Class
import Options.Applicative (Parser)
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Types as Opts

-- | Parse the given options from the command line
options :: MonadIO io => Description -> Parser a -> io a
options desc parser = liftIO
    $ Opts.execParser
    $ Opts.info (Opts.helper <*> parser)
                (Opts.header (Text.unpack (getDescription desc)))

{-| The name of a command-line argument

    This is used to infer the long name and metavariable for the command line
    flag.  For example, an `ArgName` of @\"name\"@ will create a @--name@ flag
    with a @NAME@ metavariable
-}
newtype ArgName = ArgName { getArgName :: Text }
    deriving (IsString)

{-| A brief description of what your program does

    This description will appear in the header of the @--help@ output
-}
newtype Description = Description { getDescription :: Text }
    deriving (IsString)

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
    -> Optional HelpMessage
    -> Parser Bool
switch argName helpMessage
   = Opts.switch
   $ (Opts.long . Text.unpack . getArgName) argName
  <> foldMap (Opts.help . Text.unpack . getHelpMessage) helpMessage

{- | Build an argument parser for any type by providing a `Text`-parsing
     function
-}
arg :: (Text -> Maybe a)
    -> ArgName
    -> Optional HelpMessage
    -> Parser a
arg argParse argName helpMessage
   = Opts.option (argParseToReadM argParse)
   $ Opts.metavar (Text.unpack (Text.toUpper (getArgName argName)))
  <> Opts.long (Text.unpack (getArgName argName))
  <> foldMap (Opts.help . Text.unpack . getHelpMessage) helpMessage

-- | Parse any type that implements `Read`
argRead :: Read a => ArgName -> Optional HelpMessage -> Parser a
argRead = arg (readMaybe . Text.unpack)

{-| Parse any type that implements `Integral`

    This is most commonly used to parse an `Int` or `Integer`
-}
argIntegral :: Integral n => ArgName -> Optional HelpMessage -> Parser n
argIntegral argName helpMessage = fmap fromInteger (argRead argName helpMessage)

-- | Parse a `Text` value
argText :: ArgName -> Optional HelpMessage -> Parser Text
argText = arg Just

{-| Parse any type that implements `Fractional`

    This is most commonly used to parse a `Double`
-}
argFractional :: Fractional n => ArgName -> Optional HelpMessage -> Parser n
argFractional argName helpMessage =
    fmap fromRational (argRead argName helpMessage)

argParseToReadM :: (Text -> Maybe a) -> Opts.ReadM a
argParseToReadM f = do
    s <- Opts.readerAsk
    case f (Text.pack s) of
        Just a -> return a
        Nothing -> Opts.readerAbort Opts.ShowHelpText
