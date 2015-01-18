module Turtle.Resource (
    -- * Resource
      Resource(..)
    , with

    -- * Utilities
    , readHandle
    , writeHandle

    -- * Re-exports
    , FilePath
    , Handle
    ) where

import Control.Applicative (Applicative(..), liftA2)
import Control.Exception (bracket, onException)
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Filesystem.Path (FilePath)
import qualified Filesystem
import System.IO (Handle, IOMode(ReadMode, WriteMode), hClose)
import Prelude hiding (FilePath)

import Turtle.Shell

data Resource a = Resource { acquire :: IO (a, IO ()) }

instance Functor Resource where
    fmap f r = Resource (do
        (a, release) <- acquire r
        return (f a, release) )

instance Applicative Resource where
    pure = return

    (<*>) = ap

instance Monad Resource where
    return a = Resource (return (a, return ()))

    m >>= f = Resource (do
        (a, release1) <- acquire m
        (b, release2) <- acquire (f a) `onException` release1
        return (b, release2 >> release1) )

instance MonadIO Resource where
    liftIO io = Resource (do
        a <- io
        return (a, return ()) )

instance Monoid a => Monoid (Resource a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance Num a => Num (Resource a) where
    fromInteger n = pure (fromInteger n)

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

    abs    = fmap abs
    signum = fmap signum
    negate = fmap negate

instance Fractional a => Fractional (Resource a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance Floating a => Floating (Resource a) where
    pi = pure pi

    exp   = fmap exp
    sqrt  = fmap sqrt
    log   = fmap log
    sin   = fmap sin
    tan   = fmap tan
    cos   = fmap cos
    asin  = fmap sin
    atan  = fmap atan
    acos  = fmap acos
    sinh  = fmap sinh
    tanh  = fmap tanh
    cosh  = fmap cosh
    asinh = fmap asinh
    atanh = fmap atanh
    acosh = fmap acosh

    (**)    = liftA2 (**)
    logBase = liftA2 logBase

instance IsString a => IsString (Resource a) where
    fromString str = pure (fromString str)

-- | Acquire a `Resource` within a `Shell`
with :: Resource a -> Shell a
with resource = Shell (\(FoldM step begin done) -> do
    x <- begin
    x' <- bracket (acquire resource) snd (\(a, _) -> step x a)
    done x' )

-- | Acquire a read-only `Handle` from a `FilePath`
readHandle :: FilePath -> Resource Handle
readHandle file = Resource (do
    handle <- Filesystem.openFile file ReadMode
    return (handle, hClose handle) )

-- | Acquire a write-only `Handle` from a `FilePath`
writeHandle :: FilePath -> Resource Handle
writeHandle file = Resource (do
    handle <- Filesystem.openFile file WriteMode
    return (handle, hClose handle) )
