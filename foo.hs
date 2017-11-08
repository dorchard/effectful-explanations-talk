import Control.Monad.State

fib 0 = do
  modify (+1)
  return 1

fib 1 = do
  modify (+1)
  return 1

fib n = do
  x <- fib (n - 1)
  y <- fib (n - 2)
  modify (+1)
  return (x+y)