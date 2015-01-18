module Turtle (
    -- * Shell
      Shell(..)
    , feed
    , runShell

    -- * Utilities
    , select
    , cat
    , grep

    -- * Classes
    , Applicative(..)
    , Alternative(..)
    , MonadIO(..)
    ) where

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad.IO.Class (MonadIO(..))
import Turtle.Shell (Shell(feedIO), feed, runShell)
import Turtle.Util (select, cat, grep)
