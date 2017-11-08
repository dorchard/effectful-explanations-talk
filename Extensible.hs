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
import PMonad
-- State parameterised monad
import State

-- A subset of the 'type-level-sets' package
import TypeLevelMaps

import GHC.TypeLits


get :: Get v t m => Var v -> State (Map m) (Map m) t
get v = State $ \s -> (lookp v s, s)

put :: Put v t m n => Var v -> t -> State (Map m) (Map n) ()
put v t = State $ \s -> ((), update s v t)

modify :: (Get v s m, Put v t m n) => Var v -> (s -> t) -> State (Map m) (Map n) ()
modify v f = do
  x <- get v
  put v (f x)

-- Examples

increment :: (Update "x" Int m) => State (Map m) (Map m) ()
increment = do
   (n :: Int) <- get (Var @ "x")
   put (Var @ "x") (n+1)

example :: (Update "x" Int m, Update "flag" Bool m) => State (Map m) (Map m) ()
example = do
   flag <- get (Var @ "flag")
   increment
   (n :: Int) <- get (Var @"x")
   put (Var @ "flag") ((n > 0) || flag)

go :: ((), Map '["x" ':-> Int, "flag" ':-> Bool])
go = runState example (Ext (Var @ "x") (42 :: Int) (Ext (Var @ "flag") False Empty))

example2 :: (Get "flag" Bool m, Update "x" Int m, Put "y" Int m m) => State (Map m) (Map m) ()
example2 = do
   flag <- get (Var @ "flag")
   if flag
     then modify (Var @ "x") (\(x :: Int) -> x + 1)
     else put (Var @ "y") (42 :: Int)
