-- | This is the recommended way to import this library:
--
--  > {-# LANGUAGE OverloadedStrings #-}
--  >
--  > import Turtle
--  > import Prelude hiding (FilePath)
--
--  This module re-exports the rest of the library and also re-exports useful
--  modules from @base@:
--
--  "Turtle.Pattern" provides `Pattern`s, which are like more powerful regular
--  expressions
--
--  "Turtle.Protected" provides `Protected` resources that are exception-safe
--
--  "Turtle.Shell" provides a streaming `Shell` abstraction for building
--  exception-safe pipelines
--
--  "Turtle.Prelude" provides many useful derived utilities to get you started
--  with basic shell-like programming within Haskell
--
--  "Control.Applicative" provides two classes:
--
--  * `Applicative`, which works with `Fold`, `Pattern`, `Protected`, and
--    `Shell`
--
--  * `Alternative`, which works with `Pattern` and `Shell`
--
--  "Control.Monad" provides two classes:
--
--  * `Monad`, which works with `Pattern`, `Protected` and `Shell`
--
--  * `MonadPlus`, which works with `Pattern` and `Shell`
--
--  "Control.Monad.IO.Class" provides one class:
--
--  * `MonadIO`, which works with `Protected` and `Shell`
--
--  Additionally, you might also want to import "Control.Foldl" or
--  "Control.Foldl.Text" qualified.

module Turtle (
    -- * Modules
      module Turtle.Pattern
    , module Turtle.Protected
    , module Turtle.Shell
    , module Turtle.Prelude
    , module Control.Applicative
    , module Control.Monad
    , module Control.Monad.IO.Class
    , Text
    , FilePath
    , UTCTime
    , Handle
    , Fold(..)
    , FoldM(..)
    ) where

import Turtle.Pattern
import Turtle.Protected
import Turtle.Shell
import Turtle.Prelude
import Control.Applicative
    ( Applicative(..)
    , Alternative(..)
    , (<$>)
    , liftA2
    , optional
    )
import Control.Monad
    ( MonadPlus(..)
    , forever
    , void
    , (>=>)
    , (<=<)
    , join
    , msum
    , mfilter
    , replicateM_
    , guard
    , when
    , unless
    )
import Control.Monad.IO.Class (MonadIO(..))

import Control.Foldl (Fold(..), FoldM(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Filesystem.Path (FilePath)
import System.IO (Handle)
import Prelude hiding (FilePath)
