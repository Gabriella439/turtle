{-# LANGUAGE RankNTypes #-}

{-| You can think of `Shell` as @[]@ + `IO` + `Protected`.  In fact, you can
    embed all three of them within `Shell`:

> select :: [a]         -> Shell a
> liftIO :: IO a        -> Shell a
> with   :: Protected a -> Shell a

    Those three embeddings obey these laws:

> do { x <- select m; select (f x) } = select (do { x <- m; f x })
> do { x <- liftIO m; liftIO (f x) } = liftIO (do { x <- m; f x })
> do { x <- with   m; with   (f x) } = with   (do { x <- m; f x })
>
> select (return x) = return x
> liftIO (return x) = return x
> with   (return x) = return x

    ... and `select` obeys these additional laws:

> select xs <|> select ys = select (xs <|> ys)
> select empty = empty

    You typically won't build `Shell`s using the `Shell` constructor.  Instead,
    use these functions to generate primitive `Shell`s:

    * `empty`

    * `return`

    * `select`

    * `liftIO`

    * `with`
    
    ... and use these classes to combine those primitive `Shell`s into larger
    `Shell`s:

    * `Monad` (i.e. @do@ notation)

    * `Alternative`

    If you still insist on building your own `Shell` from scratch, then the
    `Shell` you build must satisfy this law:

> -- For every shell `s`:
> foldIO s (FoldM step begin done) = do
>     x  <- step
>     x' <- foldIO s (FoldM step (return x) return)
>     done x'

    ... which is a fancy way of saying that your `Shell` must call @\'begin\'@
    exactly once when it begins and call @\'done\'@ exactly once when it ends.
-}

module Turtle.Shell (
      Shell(..)
    , fold
    , sh
    , list

    -- * Embeddings
    , select
    , liftIO
    , with
    ) where

import Control.Applicative (Applicative(..), Alternative(..), liftA2)
import Control.Exception (bracket)
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Foldl (Fold(..), FoldM(..))
import qualified Control.Foldl as Foldl
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))

import Turtle.Protected

-- | A @(Shell a)@ is a protected stream of @a@'s
newtype Shell a = Shell { foldIO :: forall r . FoldM IO a r -> IO r }

-- | Feed the stream of @a@'s produced by a `Shell` to a `Fold`
fold :: Shell a -> Fold a b -> IO b
fold s f = foldIO s (Foldl.generalize f)

-- | Run a `Shell` to completion, discarding any unused values
sh :: Shell a -> IO ()
sh s = fold s (pure ())

-- | Run a `Shell` to completion, `print`ing any unused values
list :: Show a => Shell a -> IO ()
list s = sh (do
    x <- s
    liftIO (print x) )

instance Functor Shell where
    fmap f s = Shell (\(FoldM step begin done) ->
        let step' x a = step x (f a)
        in  foldIO s (FoldM step' begin done) )

instance Applicative Shell where
    pure  = return
    (<*>) = ap

instance Monad Shell where
    return a = Shell (\(FoldM step begin done) -> do
       x  <- begin
       x' <- step x a
       done x' )

    m >>= f = Shell (\(FoldM step0 begin0 done0) -> do
        let step1 x a = foldIO (f a) (FoldM step0 (return x) return)
        foldIO m (FoldM step1 begin0 done0) )

    fail _ = mzero

instance Alternative Shell where
    empty = Shell (\(FoldM _ begin done) -> do
        x <- begin
        done x )

    s1 <|> s2 = Shell (\(FoldM step begin done) -> do
        x <- foldIO s1 (FoldM step begin return)
        foldIO s2 (FoldM step (return x) done) )

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

-- | Convert a list to `Shell` that emits each element of the list
select :: [a] -> Shell a
select  []    = empty
select (a:as) = return a <|> select as

{-| Acquire a `Protected` resource within a `Shell` in an exception-safe way

> do { x <- with m; with (f x) } = with (do { x <- m; f x })
-}
with :: Protected a -> Shell a
with resource = Shell (\(FoldM step begin done) -> do
    x <- begin
    x' <- bracket (acquire resource) snd (\(a, _) -> step x a)
    done x' )
