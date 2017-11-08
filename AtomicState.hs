-- For clarity in type classes instances
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

module AtomicState where

-- Bye Monads... as we know them
import Prelude hiding (Monad(..))
-- Hello parameterised monads
import ParameterisedMonad

newtype State s1 s2 a = State { runState :: s1 -> (a, s2) }

instance PMonad State where
  return :: a -> State s s a
  return x = State (\s -> (x, s))

  (>>=) :: State s1 s2 a -> (a -> State s2 s3 b) -> State s1 s3 b
  (State m) >>= k =
      State $ \s0 -> let (a, s1) = m s0
                         State m' = k a in m' s1

newtype Closed s = Closed s deriving Show
newtype Open   s = Open s   deriving Show

get :: State (Closed s) (Open s) s
get = State $ \(Closed s) -> (s, Open s)

put :: t -> State (Open s) (Closed t) ()
put tx = State $ \(Open _) -> ((), Closed tx)

type AtomicState s t a = State (Closed s) (Closed t) a

-----------------------------
-- Examples

myProgram :: AtomicState Int Int String
myProgram = do
  x <- get
  -- The following previous code causes a type error
  -- a <- somethingPure x
  put (x+1)
  a <- somethingPure x
  return (a ++ show x)

somethingPure :: Int -> AtomicState Int Int String
somethingPure x = do
  n <- get
  put (n + 1)
  return $ "hello" ++ show x
