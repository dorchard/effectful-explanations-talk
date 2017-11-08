-- Used to make the types extra clear
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

-- This module implement parameterised monads due to Bob Atkey
-- (see 'Parameterised Notions of Computing' JFP 2009)
-- also defined in Control.Monad.Indexed (category-extras)

module ParameterisedMonad ((>>), PMonad(..), fail, ifThenElse) where

-- Bye Monads... as we know them
import Prelude hiding (Monad(..))

-- Hello Parameterised Monads
class PMonad (p :: k -> k -> * -> *) where
  return :: a -> p inv inv a
  (>>=) :: p pre interm a -> (a -> p interm post b) -> p pre post b

-- Other boilerplate
(>>) :: PMonad p => p pre mid a -> p mid post b -> p pre post b
x >> y = x >>= const y

fail :: String -> m inv inv a
fail = error

ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y
