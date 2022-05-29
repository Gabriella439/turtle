{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

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
--  "Turtle.Shell" provides a `Shell` abstraction for building streaming,
--  exception-safe pipelines
--
--  "Turtle.Prelude" provides a library of Unix-like utilities to get you
--  started with basic shell-like programming within Haskell
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
--  "Data.Monoid" provides one class:
--
--  * `Monoid`, which works with `Fold`, `Pattern`, `Managed`, and `Shell`
--
--  "Control.Monad.Managed.Safe" provides `Managed` resources
--
--  Additionally, you might also want to import the following modules qualified:
--
--  * "Options.Applicative" from @optparse-applicative@ for command-line option
--     parsing
--
--  * "Control.Foldl" (for predefined folds)
--
--  * "Control.Foldl.Text" (for `Text`-specific folds)
--
--  * "Data.Text" (for `Text`-manipulation utilities)
--
--  * "Data.Text.IO" (for reading and writing `Text`)

module Turtle (
    -- * Modules
      module Turtle.Format
    , module Turtle.Pattern
    , module Turtle.Options
    , module Turtle.Shell
    , module Turtle.Line
    , module Turtle.Prelude
    , module Control.Applicative
    , module Control.Monad
    , module Control.Monad.IO.Class
    , module Data.Monoid
    , module Control.Monad.Managed
    , module System.FilePath
    , module Turtle.Internal
    , Fold(..)
    , FoldM(..)
    , Text
    , UTCTime
    , NominalDiffTime
    , Handle
    , ExitCode(..)
    , IsString(..)
    , (&)
    , (<&>)
    ) where

import Turtle.Format
import Turtle.Pattern
import Turtle.Options
import Turtle.Shell
import Turtle.Line
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
import Control.Monad.Managed (Managed, managed, runManaged, with)
import Control.Foldl (Fold(..), FoldM(..))
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime)
import System.FilePath (FilePath, dropExtension, hasExtension, (</>), (<.>))
import System.IO (Handle)
import System.Exit (ExitCode(..))
import Turtle.Internal
    ( root
    , directory
    , filename
    , dirname
    , basename
    , absolute
    , relative
    , stripPrefix
    , splitDirectories
    , extension
    , splitExtension
    , toText
    , fromText
    , encodeString
    , decodeString
    )
import Prelude hiding (FilePath)

#if __GLASGOW_HASKELL__ >= 710
import Data.Function ((&))
#else
infixl 1 &

-- | '&' is a reverse application operator.  This provides notational
-- convenience.  Its precedence is one higher than that of the forward
-- application operator '$', which allows '&' to be nested in '$'.
(&) :: a -> (a -> b) -> b
x & f = f x
#endif

#if __GLASGOW_HASKELL__ >= 821
import Data.Functor ((<&>))
#else
-- | Flipped version of '<$>'.
--
-- @
-- ('<&>') = 'flip' 'fmap'
-- @
--
-- @since 4.11.0.0
--
-- ==== __Examples__
-- Apply @(+1)@ to a list, a 'Data.Maybe.Just' and a 'Data.Either.Right':
--
-- >>> Just 2 <&> (+1)
-- Just 3
--
-- >>> [1,2,3] <&> (+1)
-- [2,3,4]
--
-- >>> Right 3 <&> (+1)
-- Right 4
--
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as

infixl 1 <&>
#endif
