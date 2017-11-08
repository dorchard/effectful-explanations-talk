module State where

-- Bye Monads... as we know them
import Prelude hiding (Monad(..))
import PMonad

newtype State s1 s2 a = State { runState :: s1 -> (a, s2) }

instance PMonad State where
  return x = State (\s -> (x, s))
  (State m) >>= k =
      State $ \s0 -> let (a, s1) = m s0
                         State m' = k a in m' s1
