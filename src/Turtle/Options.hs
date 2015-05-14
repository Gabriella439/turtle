{-# LANGUAGE TypeFamilies #-}

-- TODO: documentation

module Turtle.Options
    ( Opts.Parser
    , Opts.InfoMod
    , options
    , opt
    , flag
    , metavar
    , helpmsg
    , shortname
    , longname
    ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Control.Applicative
import Control.Monad.IO.Class
import qualified Options.Applicative as Opts
import qualified Options.Applicative.Types as Opts
import qualified Options.Applicative.Builder.Internal as Opts

options :: MonadIO io => Opts.Parser a -> Opts.InfoMod a -> io a
options parser info = liftIO
    (Opts.execParser (Opts.info (Opts.helper <*> parser) info))

class Option opt where
    type OptionFields opt :: * -> *
    opt :: Opts.Mod (OptionFields opt) opt -> Opts.Parser opt

instance Option Bool where
    type OptionFields Bool = Opts.FlagFields
    opt = Opts.switch

instance Option Text where
    type OptionFields Text = Opts.ArgumentFields
    opt = Opts.argument (fmap Text.pack Opts.readerAsk)

flag :: a -> a -> Opts.Mod Opts.FlagFields a -> Opts.Parser a
flag = Opts.flag

metavar :: Opts.HasMetavar f => Text -> Opts.Mod f a
metavar str = Opts.metavar (Text.unpack str)

helpmsg :: Text -> Opts.Mod f a
helpmsg str = Opts.help (Text.unpack str)

shortname :: Opts.HasName f => Char -> Opts.Mod f a
shortname = Opts.short

longname :: Opts.HasName f => Text -> Opts.Mod f a
longname str = Opts.long (Text.unpack str)
