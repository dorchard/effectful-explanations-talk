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



module TypeLevelSets where

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

class Member v t m where
  lookp :: Var v -> Map m -> t

instance {-# OVERLAPS #-} Member v t ((v ':-> t) ': m) where
  lookp _ (Ext _ x _) = x

instance Member v t m => Member v t (x ': m) where
  lookp v (Ext _ _ m) = lookp v m

class Updatable v t m n where
  update :: Map m -> Var v -> t -> Map n

instance {-# OVERLAPS #-} Updatable v t ((v ':-> s) ': m) ((v ':-> t) ': m) where
  update (Ext v _ m) _ x = Ext v x m

instance Updatable v t m n => Updatable v t ((w ':-> y) ': m) ((w ':-> y) ': n) where
  update (Ext w y m) v x = Ext w y (update m v x)

instance Updatable v t '[] '[v ':-> t] where
  update Empty v x = Ext v x Empty

type Get v t m = Member v t m
type Put v t m n = Updatable v t m n
type Update v t m = (Get v t m, Put v t m m)
