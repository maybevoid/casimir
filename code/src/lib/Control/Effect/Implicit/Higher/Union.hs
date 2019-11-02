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
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  = Union
     (Operation ops1 eff1 eff2)
     (Operation ops2 eff1 eff2)

instance
  ( Effect eff
  , EffOps ops1
  , EffOps ops2
  , Base.EffFunctor (Operation ops1 eff)
  , Base.EffFunctor (Operation ops2 eff)
  )
  => Base.EffFunctor (Union ops1 ops2 eff)
  where
    effmap _ = undefined

instance
  ( EffOps ops1
  , EffOps ops2
  , EffFunctor (Operation ops1)
  , EffFunctor (Operation ops2)
  )
  => EffFunctor (Union ops1 ops2)
   where
    invEffmap lifter contraLifter (Union ops1 ops2) =
      Union
        (invEffmap lifter contraLifter ops1)
        (invEffmap lifter contraLifter ops2)
