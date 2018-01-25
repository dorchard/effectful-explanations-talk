{-# LANGUAGE InstanceSigs #-}
module State where

-- Bye Monads... as we know them
import Prelude hiding (Monad(..))
import ParameterisedMonad

newtype State s1 s2 a = State { runState :: s1 -> (a, s2) }

-- State parameterised monad
-- ... with implementations just like the state monad
instance PMonad State where
  return :: a -> State s s a
  return x = State (\s -> (x, s))

  (>>=) :: State s1 s2 a -> (a -> State s2 s3 b) -> State s1 s3 b
  (State m) >>= k =
      State $ \s0 -> let (a, s1) = m s0
                         State m' = k a in m' s1
