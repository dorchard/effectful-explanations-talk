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

newtype State s1 s2 a = State { runState :: s1 -> (a, s2) }

instance PMonad State where
  return x = State (\s -> (x, s))
  (State m) >>= k =
      State $ \s0 -> let (a, s1) = m s0
                         State m' = k a in m' s1

get :: Member v t m => Var v -> State (Map m) (Map n) t
get v = State $ \s -> (lookp v s, s)

put :: Updatable v t m n => Var v -> t -> State (Map m) (Map n) ()
put v t = State $ \s -> ((), update s v t)

modify :: (Member v s m, Updatable v t m n) => Var v -> (s -> t) -> State (Map m) (Map n) ()
modify v f = do
  x <- get v
  put v (f x)




-- Aliases for our operations
type Get v t m = Member v t m
type Put v t m n = Updatable v t m n
type Update v t m = (Get v t m, Put v t m m)

-- Examples

increment :: (Update "x" Int m) => State (Map m) (Map m) ()
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

example2 :: (Get "flag" Bool m, Update "x" Int m, Put "y" Int m m) => State (Map m) (Map m) ()
example2 = do
   flag <- get (Var @ "flag")
   if flag
     then modify (Var @ "x") (\(x :: Int) -> x + 1)
     else put (Var @ "y") (42 :: Int)
