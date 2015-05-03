{-# LANGUAGE RankNTypes #-}

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
> foldIO s (FoldM step begin done) = do
>     x  <- begin
>     x' <- foldIO s (FoldM step (return x) return)
>     done x'

    ... which is a fancy way of saying that your `Shell` must call @\'begin\'@
    exactly once when it begins and call @\'done\'@ exactly once when it ends.
-}

module Turtle.Shell (
    -- * Shell
      Shell(..)
    , fold
    , sh
    , view

    -- * Embeddings
    , select
    , liftIO
    , using
    ) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Managed (Managed, with)
import Control.Foldl (Fold(..), FoldM(..))
import qualified Control.Foldl as Foldl
import Data.Monoid ((<>))
import Data.String (IsString(..))

-- | A @(Shell a)@ is a protected stream of @a@'s with side effects
newtype Shell a = Shell { foldIO :: forall r . FoldM IO a r -> IO r }

-- | Use a `Fold` to reduce the stream of @a@'s produced by a `Shell`
fold :: Shell a -> Fold a b -> IO b
fold s f = foldIO s (Foldl.generalize f)

-- | Run a `Shell` to completion, discarding any unused values
sh :: Shell a -> IO ()
sh s = fold s (pure ())

-- | Run a `Shell` to completion, `print`ing any unused values
view :: Show a => Shell a -> IO ()
view s = sh (do
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

-- | Shell forms a semigring, this is the closest approximation
instance Monoid a => Num (Shell a) where
    fromInteger n = select (replicate (fromInteger n) mempty)

    (+) = (<|>)
    (*) = (<>)

instance IsString a => IsString (Shell a) where
    fromString str = pure (fromString str)

-- | Convert a list to a `Shell` that emits each element of the list
select :: [a] -> Shell a
select as = Shell (\(FoldM step begin done) -> do
    x0 <- begin
    let step' a k x = do
            x' <- step x a
            k $! x'
    foldr step' done as $! x0 )

-- | Acquire a `Managed` resource within a `Shell` in an exception-safe way
using :: Managed a -> Shell a
using resource = Shell (\(FoldM step begin done) -> do
    x  <- begin
    x' <- with resource (step x)
    done x' )
