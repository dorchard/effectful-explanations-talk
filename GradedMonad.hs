{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module GradedMonad ((>>), GMonad(..), fail, ifThenElse) where

-- Bye Monads... as we know them
import Prelude hiding (Monad(..))

-- Defined in Control.Monad.Effetct (effect-monad package)
-- https://github.com/dorchard/effect-monad
class GMonad (g :: k -> * -> *) where
  type Zero g :: k
  type Plus g (x :: k) (y :: k) :: k

  return :: a -> g (Zero g) a
  (>>=) :: g x a -> (a -> g y b) -> g (Plus g x y) b


-- Some boilerplate
(>>) :: GMonad g => g x a -> g y b -> g (Plus g x y) b
x >> y = x >>= const y

fail :: String -> g x a
fail = error

-- More interesting types can be given here for graded monads,
-- see https://github.com/dorchard/effect-monad
-- Use the simple definition here
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y
