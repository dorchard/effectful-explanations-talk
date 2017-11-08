{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RebindableSyntax #-}

module Count where

{-| Provides a way to 'count' in the type-level with a monadic interface
    to sum up the individual counts of subcomputations -}
-- See https://github.com/dorchard/effect-monad

import Prelude hiding (Monad(..), map)
import GradedMonad

{-| Define type constructors for natural numbers -}
data Z
data S n

{-| The counter has no semantic meaning -}
newtype Counter n a = Counter { forget :: a }

{-| Type-level addition -}
type family n :+ m where
            n :+ Z   = n
            n :+ S m = S (n :+ m)

instance GMonad Counter where

    {-| Trivial effect annotation is 0 -}
    type Unit Counter = Z
    {-| Compose effects by addition -}
    type Plus Counter n m = n :+ m

    -- return :: a -> Counter Z a
    return = Counter

    -- (>>=) :: Counter n a -> (a -> Counter m b) -> Counter (n :+ m) b
    (Counter a) >>= k = Counter . forget $ k a

{-| A 'tick' provides a way to increment the counter -}
tick :: Counter (S Z) ()
tick = Counter ()




-- Get's very cool when combined with sized types
data Vector n a where
    Nil :: Vector Z a
    Cons :: a -> Vector n a -> Vector (S n) a

type family n :* m where
            Z   :* m = Z
            S n :* m = m :+ (n :* m)

vmap :: (a -> Counter t b) -> Vector n a -> Counter (n :* t) (Vector n b)
vmap _ Nil         = return Nil
vmap f (Cons x xs) = do
    y <- f x
    ys <- vmap f xs
    return $ Cons y ys
