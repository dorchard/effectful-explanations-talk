{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module GradedMonad ((>>), GMonad(..), fail, ifThenElse) where

-- Bye Monads... as we know them
import Prelude hiding (Monad(..))

-- Defined in Control.Monad.Indexed (category-extras)
class GMonad (g :: eff -> * -> *) where
  type Unit g :: eff
  type Plus g (x :: eff) (y :: eff) :: eff

  return :: a -> g (Unit g) a
  (>>=) :: g x a -> (a -> g y b) -> g (Plus g x y) b


-- Some boilerplate
(>>) :: GMonad g => g x a -> g y b -> g (Plus g x y) b
x >> y = x >>= const y

fail :: String -> g x a
fail = error

class GMonadJoin (g :: eff -> * -> *) where
   type Join g (x :: eff) (y :: eff) :: eff
   join :: Bool -> g x a -> g y a -> g (Join g x y) a

class IfThenElse a b c where
  ifThenElse :: Bool -> a -> b -> c

instance IfThenElse a a a where
  ifThenElse True x _ = x
  ifThenElse False _ y = y

instance (GMonadJoin g, z ~ Join g x y) => IfThenElse (g x a) (g y a) (g z a) where
  ifThenElse = join
