import Control.Monad.State

myProgram :: State Int String
myProgram = do
  x <- get
  a <- somethingUmPurish x
  put (x+1)
  return (a ++ show x)

somethingPure :: Int -> String
somethingPure x = "hello" ++ show x

somethingUmPurish :: Int -> State Int String
somethingUmPurish x = do
  n <- get
  put (n + 1)
  return $ "hello" ++ show x
