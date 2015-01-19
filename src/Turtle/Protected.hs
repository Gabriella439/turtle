{-| This module handles exception-safety.

    You can build `Protected` resources using `Protect`:

> readhandle :: FilePath -> Protected Handle
> readhandle file = Protect (do
>     handle <- Filesystem.openFile file ReadMode
>     return (handle, hClose handle) )

    You can combine `Protected` resources using @do@ notation:

> twoFiles :: Protected (Handle, Handle)
> twoFiles = do
>     handle1 <- readhandle "file1.txt"
>     handle2 <- readhandle "file2.txt"
>     return (handle1, handle2)

    You can consume `Protected` resources with `using`:

> using (readhandle "file.txt") (\handle -> ...)

    ... or you can acquire `Protected` resources within a `Turtle.Shell.Shell`
    using `Turtle.Shell.with`:

> example = do
>     handle <- with (readhandle "file.txt")
>     ...
 
-}

module Turtle.Protected (
    -- * Protected
      Protected(..)
    , using
    ) where

import Control.Applicative (Applicative(..), liftA2)
import Control.Exception (bracket, onException)
import Control.Monad (ap)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))

{-| A `Protected` resource of type @a@
-}
data Protected a = Protect { acquire :: IO (a, IO ()) }

instance Functor Protected where
    fmap f r = Protect (do
        (a, release) <- acquire r
        return (f a, release) )

instance Applicative Protected where
    pure = return

    (<*>) = ap

instance Monad Protected where
    return a = Protect (return (a, return ()))

    m >>= f = Protect (do
        (a, release1) <- acquire m
        (b, release2) <- acquire (f a) `onException` release1
        return (b, release2 >> release1) )

instance MonadIO Protected where
    liftIO io = Protect (do
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

-- | Acquire a `Protected` resource using `bracket`
using :: Protected a -> (a -> IO r) -> IO r
using p k = bracket (acquire p) (\(_, release) -> release) (\(a, _) -> k a)
