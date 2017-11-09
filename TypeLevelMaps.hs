{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

-- Taken from the `type-level-sets` package
-- Data.Type.Map
-- https://github.com/dorchard/type-level-sets
-- https://hackage.haskell.org/package/type-level-sets
module TypeLevelMaps where

import GHC.TypeLits

data Var (v :: Symbol) = Var

instance KnownSymbol v => Show (Var v) where
   show = symbolVal

-- Mappings
infixr 9 :->
{-| A key-value pair -}
data Mapping k v = k :-> v

data Map (n :: [Mapping Symbol *]) where
    Empty :: Map '[]
    Ext :: Var v -> t -> Map m -> Map ((v ':-> t) ': m)

-- Showing map nicely
instance Show (Map '[]) where
    show Empty = "{}"

instance (KnownSymbol k, Show v, Show' (Map s)) => Show (Map ((k ':-> v) ': s)) where
    show (Ext k v s) = "{" ++ show k ++ " :-> " ++ show v ++ show' s ++ "}"

class Show' t where
    show' :: t -> String
instance Show' (Map '[]) where
    show' Empty = ""
instance (KnownSymbol k, Show v, Show' (Map s)) => Show' (Map ((k ':-> v) ': s)) where
    show' (Ext k v s) = ", " ++ show k ++ " :-> " ++ show v ++ show' s


-- Looking up from a map, and IsMembership
class IsMember v t m where
  lookp :: Var v -> Map m -> t

instance {-# OVERLAPS #-} IsMember v t ((v ':-> t) ': m) where
  lookp _ (Ext _ x _) = x

instance IsMember v t m => IsMember v t (x ': m) where
  lookp v (Ext _ _ m) = lookp v m


-- Updating a map
class Updatable v t m n where
  update :: Map m -> Var v -> t -> Map n

instance {-# OVERLAPS #-} Updatable v t ((v ':-> s) ': m) ((v ':-> t) ': m) where
  update (Ext v _ m) _ x = Ext v x m

instance Updatable v t m n => Updatable v t ((w ':-> y) ': m) ((w ':-> y) ': n) where
  update (Ext w y m) v x = Ext w y (update m v x)

instance Updatable v t '[] '[v ':-> t] where
  update Empty v x = Ext v x Empty
