{-# LANGUAGE RankNTypes #-}

{-| All `Shell`s must satisfy this law:

> feedIO s (FoldM step begin done) = do
>     x  <- step
>     x' <- feedIO s (FoldM step (return x) return)
>     done x'

-}

module Turtle.Shell (
      Shell(..)
    , feed
    , runShell

    -- * Re-exports
    , Fold(..)
    , FoldM(..)
    ) where

import Control.Applicative (Applicative(..), Alternative(..), liftA2)
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Foldl (Fold(..), FoldM(..))
import qualified Control.Foldl as Foldl
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))

-- | A @(`Shell` a)@ is a stream of @a@'s
newtype Shell a = Shell { feedIO :: forall r . FoldM IO a r -> IO r }

feed :: Shell a -> Fold a b -> IO b
feed s f = feedIO s (Foldl.generalize f)

runShell :: Shell a -> IO ()
runShell s = feed s (pure ())

instance Functor Shell where
    fmap f s = Shell (\(FoldM step begin done) ->
        let step' x a = step x (f a)
        in  feedIO s (FoldM step' begin done) )

instance Applicative Shell where
    pure  = return
    (<*>) = ap

instance Monad Shell where
    return a = Shell (\(FoldM step begin done) -> do
       x  <- begin
       x' <- step x a
       done x' )

    m >>= f = Shell (\(FoldM step0 begin0 done0) -> do
        let step1 x a = feedIO (f a) (FoldM step0 (return x) return)
        feedIO m (FoldM step1 begin0 done0) )

    fail _ = mzero

instance Alternative Shell where
    empty = Shell (\(FoldM _ begin done) -> do
        x <- begin
        done x )

    s1 <|> s2 = Shell (\(FoldM step begin done) -> do
        x <- feedIO s1 (FoldM step begin return)
        feedIO s2 (FoldM step (return x) done) )

instance MonadPlus Shell where
    mzero = empty

    mplus = (<|>)

instance MonadIO Shell where
    liftIO io = Shell (\(FoldM step begin done) -> do
        x  <- begin
        a  <- io
        x' <- step x a
        done x' )

instance Monoid a => Monoid (Shell a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

instance Num a => Num (Shell a) where
    fromInteger n = pure (fromInteger n)

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

    abs    = fmap abs
    signum = fmap signum
    negate = fmap negate

instance Fractional a => Fractional (Shell a) where
    fromRational n = pure (fromRational n)

    recip = fmap recip

    (/) = liftA2 (/)

instance Floating a => Floating (Shell a) where
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

instance IsString a => IsString (Shell a) where
    fromString str = pure (fromString str)
