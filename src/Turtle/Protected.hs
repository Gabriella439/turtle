module Turtle.Protected (
    -- * Protected
      Protected(..)
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

-- | A `Protected` resource of type @a@
data Protected a = Protected { acquire :: IO (a, IO ()) }

instance Functor Protected where
    fmap f r = Protected (do
        (a, release) <- acquire r
        return (f a, release) )

instance Applicative Protected where
    pure = return

    (<*>) = ap

instance Monad Protected where
    return a = Protected (return (a, return ()))

    m >>= f = Protected (do
        (a, release1) <- acquire m
        (b, release2) <- acquire (f a) `onException` release1
        return (b, release2 >> release1) )

instance MonadIO Protected where
    liftIO io = Protected (do
        a <- io
        return (a, return ()) )

instance Monoid a => Monoid (Protected a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance Num a => Num (Protected a) where
    fromInteger n = pure (fromInteger n)

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

    abs    = fmap abs
    signum = fmap signum
    negate = fmap negate

instance Fractional a => Fractional (Protected a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance Floating a => Floating (Protected a) where
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

instance IsString a => IsString (Protected a) where
    fromString str = pure (fromString str)

-- | Acquire a `Protected` resource within a `Shell`
with :: Protected a -> Shell a
with resource = Shell (\(FoldM step begin done) -> do
    x <- begin
    x' <- bracket (acquire resource) snd (\(a, _) -> step x a)
    done x' )

-- | Acquire a `Protected` read-only `Handle` from a `FilePath`
readHandle :: FilePath -> Protected Handle
readHandle file = Protected (do
    handle <- Filesystem.openFile file ReadMode
    return (handle, hClose handle) )

-- | Acquire a `Protected` write-only `Handle` from a `FilePath`
writeHandle :: FilePath -> Protected Handle
writeHandle file = Protected (do
    handle <- Filesystem.openFile file WriteMode
    return (handle, hClose handle) )
