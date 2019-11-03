{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.Union
where

import Data.Kind

import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.EffFunctor
import qualified Control.Effect.Implicit.Base as Base

infixr 7 ⊎
type (⊎) = Union

data Union ops1 ops2

data UnionOps ops1 ops2
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  = UnionOps
     (ops1 eff1 eff2)
     (ops2 eff1 eff2)

instance
  ( Effect eff
  , Base.EffFunctor (ops1 eff)
  , Base.EffFunctor (ops2 eff)
  )
  => Base.EffFunctor (UnionOps ops1 ops2 eff)
  where
    effmap lifter (UnionOps ops1 ops2) =
      UnionOps
        (Base.effmap lifter ops1)
        (Base.effmap lifter ops2)

instance
  ( EffFunctor ops1
  , EffFunctor ops2
  )
  => EffFunctor (UnionOps ops1 ops2)
   where
    invEffmap lifter contraLift (UnionOps ops1 ops2) =
      UnionOps
        (invEffmap lifter contraLift ops1)
        (invEffmap lifter contraLift ops2)
