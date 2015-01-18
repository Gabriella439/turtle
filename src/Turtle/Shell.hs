{-# LANGUAGE RankNTypes #-}

{- All `Shell`s must satisfy this law:

> foldM_ s step begin done = do
>     x  <- step
>     x' <- foldM_ s step (return x) return
>     done x'

-}

module Turtle.Shell (
      Shell(..)
    , fold
    , foldIO
    , runShell

    -- * Re-exports
    , Fold(..)
    , FoldM(..)
    ) where

import Control.Applicative (Applicative(..), Alternative(..), liftA2)
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Foldl (Fold, FoldM)
import qualified Control.Foldl as Foldl
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))

-- | A @(`Shell` a)@ is a stream of @a@'s
newtype Shell a = Shell
    { foldM_ :: forall r x . (x -> a -> IO x) -> IO x -> (x -> IO r) -> IO r }

fold :: Fold a b -> Shell a -> IO b
fold f = foldIO (Foldl.generalize f)

foldIO :: FoldM IO a b -> Shell a -> IO b
foldIO f s = Foldl.impurely (foldM_ s) f

runShell :: Shell a -> IO ()
runShell = fold (pure ())

instance Functor Shell where
    fmap f s = Shell (\step begin done ->
        let step' x a = step x (f a)
        in  foldM_ s step' begin done )

instance Applicative Shell where
    pure  = return
    (<*>) = ap

instance Monad Shell where
    return a = Shell (\step begin done -> do
       x  <- begin
       x' <- step x a
       done x' )

    m >>= f = Shell (\step0 begin0 done0 -> do
        let step1 x a = foldM_ (f a) step0 (return x) return
        foldM_ m step1 begin0 done0 )

    fail _ = mzero

instance Alternative Shell where
    empty = Shell (\_ begin done -> do
        x <- begin
        done x )

    s1 <|> s2 = Shell (\step begin done -> do
        x <- foldM_ s1 step begin return
        foldM_ s2 step (return x) done )

instance MonadPlus Shell where
    mzero = empty

    mplus = (<|>)

instance MonadIO Shell where
    liftIO io = Shell (\step begin done -> do
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
