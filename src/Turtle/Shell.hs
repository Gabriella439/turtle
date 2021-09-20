{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-| You can think of `Shell` as @[]@ + `IO` + `Managed`.  In fact, you can embed
    all three of them within a `Shell`:

> select ::        [a] -> Shell a
> liftIO ::      IO a  -> Shell a
> using  :: Managed a  -> Shell a

    Those three embeddings obey these laws:

> do { x <- select m; select (f x) } = select (do { x <- m; f x })
> do { x <- liftIO m; liftIO (f x) } = liftIO (do { x <- m; f x })
> do { x <- with   m; using  (f x) } = using  (do { x <- m; f x })
>
> select (return x) = return x
> liftIO (return x) = return x
> using  (return x) = return x

    ... and `select` obeys these additional laws:

> select xs <|> select ys = select (xs <|> ys)
> select empty = empty

    You typically won't build `Shell`s using the `Shell` constructor.  Instead,
    use these functions to generate primitive `Shell`s:

    * `empty`, to create a `Shell` that outputs nothing

    * `return`, to create a `Shell` that outputs a single value

    * `select`, to range over a list of values within a `Shell`

    * `liftIO`, to embed an `IO` action within a `Shell`

    * `using`, to acquire a `Managed` resource within a `Shell`
    
    Then use these classes to combine those primitive `Shell`s into larger
    `Shell`s:

    * `Alternative`, to concatenate `Shell` outputs using (`<|>`)

    * `Monad`, to build `Shell` comprehensions using @do@ notation

    If you still insist on building your own `Shell` from scratch, then the
    `Shell` you build must satisfy this law:

> -- For every shell `s`:
> _foldShell s (FoldShell step begin done) = do
>     x' <- _foldShell s (FoldShell step begin return)
>     done x'

    ... which is a fancy way of saying that your `Shell` must call @\'begin\'@
    exactly once when it begins and call @\'done\'@ exactly once when it ends.
-}

module Turtle.Shell (
    -- * Shell
      Shell(..)
    , FoldShell(..)
    , _foldIO
    , _Shell
    , foldIO
    , foldShell
    , fold
    , reduce
    , sh
    , view

    -- * Embeddings
    , select
    , liftIO
    , using
    , fromIO
    ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.Catch (MonadThrow(..), MonadCatch(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Managed (MonadManaged(..), with)
import qualified Control.Monad.Fail as Fail
import Control.Foldl (Fold(..), FoldM(..))
import qualified Control.Foldl as Foldl
import Data.Foldable (Foldable)
import qualified Data.Foldable
import Data.Monoid
import Data.String (IsString(..))
import Prelude -- Fix redundant import warnings

{-| This is similar to @`Control.Foldl.FoldM` `IO`@ except that the @begin@
    field is pure

    This small difference is necessary to implement a well-behaved `MonadCatch`
    instance for `Shell`
-}
data FoldShell a b = forall x . FoldShell (x -> a -> IO x) x (x -> IO b)

-- | A @(Shell a)@ is a protected stream of @a@'s with side effects
newtype Shell a = Shell { _foldShell:: forall r . FoldShell a r -> IO r }

data Maybe' a = Just' !a | Nothing'

translate :: FoldM IO a b -> FoldShell a b
translate (FoldM step begin done) = FoldShell step' Nothing' done'
  where
    step' Nothing' a = do
        x  <- begin
        x' <- step x a
        return $! Just' x'
    step' (Just' x) a = do
        x' <- step x a
        return $! Just' x'

    done' Nothing' = do
        x <- begin
        done x
    done' (Just' x) = do
        done x

-- | Use a @`FoldM` `IO`@ to reduce the stream of @a@'s produced by a `Shell`
foldIO :: MonadIO io => Shell a -> FoldM IO a r -> io r
foldIO s f = liftIO (_foldIO s f)

{-| Provided for backwards compatibility with versions of @turtle-1.4.*@ and
    older
-}
_foldIO :: Shell a -> FoldM IO a r -> IO r
_foldIO s foldM = _foldShell s (translate foldM)

-- | Provided for ease of migration from versions of @turtle-1.4.*@ and older
_Shell :: (forall r . FoldM IO a r -> IO r) -> Shell a
_Shell f = Shell (f . adapt)
  where
    adapt (FoldShell step begin done) = FoldM step (return begin) done

-- | Use a `FoldShell` to reduce the stream of @a@'s produced by a `Shell`
foldShell :: MonadIO io => Shell a -> FoldShell a b -> io b
foldShell s f = liftIO (_foldShell s f)

-- | Use a `Fold` to reduce the stream of @a@'s produced by a `Shell`
fold :: MonadIO io => Shell a -> Fold a b -> io b
fold s f = foldIO s (Foldl.generalize f)

-- | Flipped version of 'fold'. Useful for reducing a stream of data
--
-- ==== __Example__
-- Sum a `Shell` of numbers:
--
-- >>> select [1, 2, 3] & reduce Fold.sum
-- 6
reduce :: MonadIO io => Fold a b -> Shell a -> io b
reduce = flip fold

-- | Run a `Shell` to completion, discarding any unused values
sh :: MonadIO io => Shell a -> io ()
sh s = fold s (pure ())

-- | Run a `Shell` to completion, `print`ing any unused values
view :: (MonadIO io, Show a) => Shell a -> io ()
view s = sh (do
    x <- s
    liftIO (print x) )

instance Functor Shell where
    fmap f s = Shell (\(FoldShell step begin done) ->
        let step' x a = step x (f a)
        in  _foldShell s (FoldShell step' begin done) )

instance Applicative Shell where
    pure  = return
    (<*>) = ap

instance Monad Shell where
    return a = Shell (\(FoldShell step begin done) -> do
       x <- step begin a
       done x )

    m >>= f = Shell (\(FoldShell step0 begin0 done0) -> do
        let step1 x a = _foldShell (f a) (FoldShell step0 x return)
        _foldShell m (FoldShell step1 begin0 done0) )

#if!(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
#endif

instance Alternative Shell where
    empty = Shell (\(FoldShell _ begin done) -> done begin)

    s1 <|> s2 = Shell (\(FoldShell step begin done) -> do
        x <- _foldShell s1 (FoldShell step begin return)
        _foldShell s2 (FoldShell step x done) )

instance MonadPlus Shell where
    mzero = empty

    mplus = (<|>)

instance MonadIO Shell where
    liftIO io = Shell (\(FoldShell step begin done) -> do
        a <- io
        x <- step begin a
        done x )

instance MonadManaged Shell where
    using resource = Shell (\(FoldShell step begin done) -> do
        x <- with resource (step begin)
        done x )

instance MonadThrow Shell where
    throwM e = Shell (\_ -> throwM e)

instance MonadCatch Shell where
    m `catch` k = Shell (\f-> _foldShell m f `catch` (\e -> _foldShell (k e) f))

instance Fail.MonadFail Shell where
    fail _ = mzero

#if __GLASGOW_HASKELL__ >= 804
instance Monoid a => Semigroup (Shell a) where
  (<>) = mappend
#endif

instance Monoid a => Monoid (Shell a) where
    mempty  = pure mempty
    mappend = liftA2 mappend

-- | Shell forms a semiring, this is the closest approximation
instance Monoid a => Num (Shell a) where
    fromInteger n = select (replicate (fromInteger n) mempty)

    (+) = (<|>)
    (*) = (<>)

instance IsString a => IsString (Shell a) where
    fromString str = pure (fromString str)

-- | Convert a list to a `Shell` that emits each element of the list
select :: Foldable f => f a -> Shell a
select as = Shell (\(FoldShell step begin done) -> do
    let step' a k x = do
            x' <- step x a
            k $! x'
    Data.Foldable.foldr step' done as $! begin )

-- | Convert an `IO` action that returns a `Maybe` into a `Shell`
fromIO :: IO (Maybe a) -> Shell a
fromIO io =
    Shell
        (\(FoldShell step begin done) -> do
            let loop x = do
                    m <- io
                    case m of
                        Just a -> do
                            x' <- step x a
                            loop x
                        Nothing -> do
                            done x

            loop begin
        )
