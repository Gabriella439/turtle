-- | See "Turtle.Tutorial" to learn how to use this library or "Turtle.Prelude"
--  for a quick-start guide.
--
--  Here is the recommended way to import this library:
--
--  > {-# LANGUAGE OverloadedStrings #-}
--  >
--  > import Turtle
--  > import Prelude hiding (FilePath)
--
--  This module re-exports the rest of the library and also re-exports useful
--  modules from @base@:
--
--  "Turtle.Format" provides type-safe string formatting
--
--  "Turtle.Pattern" provides `Pattern`s, which are like more powerful regular
--  expressions
--
--  "Turtle.Shell" provides a streaming `Shell` abstraction for building
--  exception-safe pipelines
--
--  "Turtle.Prelude" provides many useful derived utilities to get you started
--  with basic shell-like programming within Haskell
--
--  "Control.Applicative" provides two classes:
--
--  * `Applicative`, which works with `Fold`, `Pattern`, `Managed`, and `Shell`
--
--  * `Alternative`, which works with `Pattern` and `Shell`
--
--  "Control.Monad" provides two classes:
--
--  * `Monad`, which works with `Pattern`, `Managed` and `Shell`
--
--  * `MonadPlus`, which works with `Pattern` and `Shell`
--
--  "Control.Monad.IO.Class" provides one class:
--
--  * `MonadIO`, which works with `Managed` and `Shell`
--
--  "Data.Monoid" provides on class:
--
--  * `Monoid`, which works with `Fold`, `Pattern`, `Managed`, and `Shell`
--
--  "Control.Monad.Managed.Safe" provides `Managed` resources
--
--  "Filesystem.Path.CurrentOS" provides `FilePath`-manipulation utilities
--
--  "Control.Foldl" provides the `Fold` \/ `FoldM` types
--
--  Additionally, you might also want to import the following modules qualified:
--
--  * "Options.Applicative" from @optparse-applicative@ for command-line option
--     parsing
--
--  * "Control.Foldl" (for folds)
--
--  * "Control.Foldl.Text" (for `Text`-specific folds)
--
--  * "Data.Text" (for `Text`-manipulation utilities)
--
--  * "Data.Text.IO" (for reading and writing `Text`)
--
--  * "Filesystem.Path.CurrentOS" (for the remaining `FilePath` utilities)

module Turtle (
    -- * Modules
      module Turtle.Format
    , module Turtle.Pattern
    , module Turtle.Shell
    , module Turtle.Prelude
    , module Control.Applicative
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Data.Monoid
    , module Control.Monad.Managed.Safe
    , module Filesystem.Path.CurrentOS
    , module Control.Foldl
    , Text
    , UTCTime
    , Handle
    , ExitCode(..)
    , IsString(..)
    ) where

import Turtle.Format
import Turtle.Pattern
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
import Data.Monoid (Monoid(..), (<>))
import Data.String (IsString(..))
import Filesystem.Path.CurrentOS
    ( FilePath
    , root
    , directory
    , parent
    , filename
    , dirname
    , basename
    , absolute
    , relative
    , (</>)
    , commonPrefix
    , stripPrefix
    , collapse
    , splitDirectories
    , extension
    , hasExtension
    , (<.>)
    , dropExtension
    , splitExtension
    , toText
    , fromText
    )
import Control.Monad.Managed.Safe (Managed, managed, runManaged)
import Control.Foldl (Fold(..), FoldM(..))
import Data.Text (Text)
import Data.Time (UTCTime)
import System.IO (Handle)
import System.Exit (ExitCode(..))
import Prelude hiding (FilePath)
