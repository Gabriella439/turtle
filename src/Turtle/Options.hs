{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO: documentation

module Turtle.Options
    ( Parser
    , ParameterName(..)
    , LongName(..)
    , HelpMessage(..)
    , options
    , switch
    , parameter
    , parameterAuto
    , parameterText
    , parameterInteger
    , parameterInt
    , parameterDouble
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

newtype ParameterName = ParameterName { getParameterName :: Text }
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

parameter
    :: (Text -> Maybe a)
    -> ParameterName
    -> Optional HelpMessage
    -> Parser a
parameter paramRead paramName helpMessage
   = Opts.argument (parameterReadToReadM paramRead)
   $ Opts.metavar (Text.unpack (getParameterName paramName))
  <> foldMap (Opts.help . Text.unpack . getHelpMessage) helpMessage

parameterAuto :: Read a => ParameterName -> Optional HelpMessage -> Parser a
parameterAuto = parameter (readMaybe . Text.unpack)

parameterText :: ParameterName -> Optional HelpMessage -> Parser Text
parameterText = parameter Just

parameterInteger :: ParameterName -> Optional HelpMessage -> Parser Integer
parameterInteger = parameterAuto

parameterInt :: ParameterName -> Optional HelpMessage -> Parser Int
parameterInt = parameterAuto

parameterDouble :: ParameterName -> Optional HelpMessage -> Parser Double
parameterDouble = parameterAuto

parameterReadToReadM :: (Text -> Maybe a) -> Opts.ReadM a
parameterReadToReadM f = do
    s <- Opts.readerAsk
    case f (Text.pack s) of
        Just a -> return a
        Nothing -> Opts.readerAbort Opts.ShowHelpText
