{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.Union
where

import Data.Kind

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.EffFunctor

infixr 7 ⊎
type (⊎) = HUnion

data HUnion ops1 ops2
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  = HUnion
     (ops1 eff1 eff2)
     (ops2 eff1 eff2)

instance
  (Effect eff, EffFunctor (ops1 eff), EffFunctor (ops2 eff))
  => EffFunctor (HUnion ops1 ops2 eff)
  where
    effmap _ = undefined

instance
  (HigherEffFunctor ops1, HigherEffFunctor ops2)
  => HigherEffFunctor (HUnion ops1 ops2)
   where
    invEffmap lifter contraLifter (HUnion ops1 ops2) =
      HUnion
        (invEffmap lifter contraLifter ops1)
        (invEffmap lifter contraLifter ops2)

    contraEffmap lifter contraLifter (HUnion ops1 ops2) =
      HUnion
        (contraEffmap lifter contraLifter ops1)
        (contraEffmap lifter contraLifter ops2)
