-- For clarity in type classes instances
{-# LANGUAGE InstanceSigs #-}

-- We're not in Kansas anymore...
{-# LANGUAGE RebindableSyntax #-}

module AtomicState where

-- Bye Monads... as we know them
import Prelude hiding (Monad(..))
-- Hello parameterised monads
import ParameterisedMonad
import State

newtype Closed s = Closed s deriving Show
newtype Open   s = Open s   deriving Show


-- get :: State s s
get :: State (Closed s) (Open s) s
get = State $ \(Closed s) -> (s, Open s)

-- put :: s -> State s ()
put :: t -> State (Open s) (Closed t) ()
put tx = State $ \(Open _) -> ((), Closed tx)

-- modify :: (t -> t) -> State s ()
modify :: (s -> t) -> State (Closed s) (Closed t) ()
modify f = get >>= (put . f)

-----------------------------
-- Examples

myProgram :: State (Closed Int) (Closed Int) String
myProgram = do
  x <- get
  put (x+1)
  a <- somethingPurish x
  return (a ++ show x)

somethingPurish :: Int -> State (Closed Int) (Closed Int) String
somethingPurish n = do
  x <- get
  put (x + 1)
  return $ if n == 0 then "hello" else "goodbye"
