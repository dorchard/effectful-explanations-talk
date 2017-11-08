{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Extensible where

-- Bye Monads... as we know them
import Prelude hiding (Monad(..))
-- Parameterised monad
import ParameterisedMonad
-- State parameterised monad
--import State
-- A subset of the 'type-level-sets' package
import TypeLevelMaps


-- Example map
exMap :: Map '["x" ':-> Int, "flag" ':-> Bool]
exMap = Ext (Var @ "x") (42 :: Int)
      $ Ext (Var @ "flag") False
        Empty

-- Fine-grained get, put, and modify

data State s1 s2 a where
      Full :: (Map s1 -> (a, Map s2)) -> State s1 s2 a
      Get  :: (Map s1 -> a) -> State s1 s2 a
      Pure :: a -> State s s a

instance PMonad State where
  return x = Pure x
  (Pure x) >>= k = k x
  (Get  m) >>= k =
     Full $ \s0 ->
       let a = m s0
       in case k a of
            Full m' -> m' s0
            Get  m' -> (m' s0, s0)
            Pure x  -> (x, s0)
  (Full m) >>= k =
      Full $ \s0 -> let (a, s1) = m s0
                     in case k a of
                          Full m' -> m' s1
                          Get  m' -> (m' s1, s1)
                          Pure x  -> (x, s1)

get :: Member v t m => Var v -> State m n t
get v = State $ \s -> (lookp v s, s)

put :: Updatable v t m n => Var v -> t -> State m n ()
put v t = State $ \s -> ((), update s v t)

modify :: (Member v s m, Updatable v t m n) => Var v -> (s -> t) -> State m n ()
modify v f = do
  x <- get v
  put v (f x)




-- Aliases for our operations
type Get v t m = Member v t m
type Put v t m n = Updatable v t m n
type Update v t m = (Get v t m, Put v t m m)

-- Examples

increment :: (Update "x" Int m) => State m m ()
increment = do
   (n :: Int) <- get (Var @ "x")
   put (Var @ "x") (n+1)

example = do
   flag <- get (Var @ "flag")
   increment
   (n :: Int) <- get (Var @"x")
   return ((n > 0) || flag)


go :: (Bool, Map '["x" ':-> Int])
go = runState example exMap

example2 :: (Get "flag" Bool m, Update "x" Int m, Put "y" Int m m) => State m m ()
example2 = do
   flag <- get (Var @ "flag")
   if flag
     then modify (Var @ "x") (\(x :: Int) -> x + 1)
     else put (Var @ "y") (42 :: Int)
