{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO: documentation

module Turtle.Options
    ( Parser
    , ArgName(..)
    , LongName(..)
    , HelpMessage(..)
    , options
    , switch
    , arg
    , argRead
    , argText
    , argInteger
    , argInt
    , argDouble
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

options :: MonadIO io => Text -> Parser a -> io a
options header parser = liftIO
    $ Opts.execParser
    $ Opts.info (Opts.helper <*> parser) (Opts.header (Text.unpack header))

newtype ArgName = ArgName { getArgName :: Text }
    deriving (IsString)

newtype LongName = LongName { getLongName :: Text }
    deriving (IsString)

newtype HelpMessage = HelpMessage { getHelpMessage :: Text }
    deriving (IsString)

switch
    :: LongName
    -> Optional HelpMessage
    -> Parser Bool
switch longName helpMessage
   = Opts.switch
   $ (Opts.long . Text.unpack . getLongName) longName
  <> foldMap (Opts.short . fst) (Text.uncons (getLongName longName))
  <> foldMap (Opts.help . Text.unpack . getHelpMessage) helpMessage

arg :: (Text -> Maybe a)
    -> ArgName
    -> Optional HelpMessage
    -> Parser a
arg argParse argName helpMessage
   = Opts.argument (argParseToReadM argParse)
   $ Opts.metavar (Text.unpack (getArgName argName))
  <> foldMap (Opts.help . Text.unpack . getHelpMessage) helpMessage

argRead :: Read a => ArgName -> Optional HelpMessage -> Parser a
argRead = arg (readMaybe . Text.unpack)

argText :: ArgName -> Optional HelpMessage -> Parser Text
argText = arg Just

argInteger :: ArgName -> Optional HelpMessage -> Parser Integer
argInteger = argRead

argInt :: ArgName -> Optional HelpMessage -> Parser Int
argInt = argRead

argDouble :: ArgName -> Optional HelpMessage -> Parser Double
argDouble = argRead

argParseToReadM :: (Text -> Maybe a) -> Opts.ReadM a
argParseToReadM f = do
    s <- Opts.readerAsk
    case f (Text.pack s) of
        Just a -> return a
        Nothing -> Opts.readerAbort Opts.ShowHelpText
