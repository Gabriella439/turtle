{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- TODO: documentation

module Turtle.Options
    ( Parser
    , ParameterRead
    , parameterRead
    , ParameterName(..)
    , LongName(..)
    , ShortName(..)
    , HelpMessage(..)
    , options
    , switch
    , parameter
    , pAuto
    , pText
    , pInteger
    , pDouble
    ) where

import Data.Monoid
import Data.String (IsString)
import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Optional
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
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

newtype ShortName = ShortName { getShortName :: Char }

newtype HelpMessage = HelpMessage { getHelpMessage :: Text }
    deriving (IsString)

optionalOr :: Monoid m  => (a -> m) -> Optional a -> m
optionalOr _ Default = mempty
optionalOr f (Specific a) = f a

switch
    :: LongName
    -> Optional ShortName
    -> Optional HelpMessage
    -> Parser Bool
switch longName shortName helpMessage
   = Opts.switch
   $ (Opts.long . Text.unpack . getLongName) longName
  <> optionalOr (Opts.short . getShortName) shortName
  <> optionalOr (Opts.help . Text.unpack . getHelpMessage) helpMessage

parameter
    :: ParameterRead a
    -> ParameterName
    -> Optional HelpMessage
    -> Parser a
parameter paramRead paramName helpMessage
   = Opts.option (parameterReadToReadM paramRead)
   $ Opts.metavar (Text.unpack (getParameterName paramName))
  <> optionalOr (Opts.help . Text.unpack . getHelpMessage) helpMessage

newtype ParameterRead a = ParameterRead (ReaderT String Maybe a)
    deriving (Functor, Applicative, Monad)

parameterRead :: (Text -> Maybe a) -> ParameterRead a
parameterRead f = ParameterRead (ReaderT (f . Text.pack))

pAuto :: Read a => ParameterRead a
pAuto = ParameterRead (ReaderT readMaybe)

pText :: ParameterRead Text
pText = ParameterRead (ReaderT $ \s -> Just (Text.pack s))

pInteger :: ParameterRead Integer
pInteger = pAuto

pInt :: ParameterRead Int
pInt = pAuto

pDouble :: ParameterRead Double
pDouble = pAuto

parameterReadToReadM :: ParameterRead a -> Opts.ReadM a
parameterReadToReadM (ParameterRead f) = do
    s <- Opts.readerAsk
    case runReaderT f s of
        Just a -> return a
        Nothing -> Opts.readerAbort Opts.ShowHelpText
