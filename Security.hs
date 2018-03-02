{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RebindableSyntax #-}

import GradedMonad
import Prelude hiding (Monad(..))

data Lattice = Public | Private
-- where Public <= Private

data Level (l :: Lattice) a = Level { unwrap :: a }
  deriving Show

type family Join (l :: Lattice) (l' :: Lattice) :: Lattice where
            Join Private x = Private
            Join x Private = Private
            Join Public Public = Public

instance GMonad Level where
  type Zero Level = Public
  type Plus Level l l' = Join l l'

  return :: a -> Level Public a
  return x = Level x

  (>>=) :: Level l a -> (a -> Level l' b) -> Level (Join l l') b
  (Level x) >>= k = let (Level y) = k x in Level y

upcast :: Level Public a -> Level Private a
upcast (Level x) = Level x

run :: Level Public a -> a
run = unwrap

--------------------------------------------------------

secretPin :: Level Private Int
secretPin = upcast (return 12345)

hash :: Int -> Level Public Int
hash x = return (x * x * x)

salt = do
  pin <- secretPin
  h   <- hash pin
  return (h + 1234)