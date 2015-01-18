module Turtle (
    -- * Shell
      Shell
    , fold
    , foldIO
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
import Turtle.Shell (Shell, fold, runShell)
import Turtle.Util (select, cat, grep)
