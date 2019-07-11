{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.Union
where

import Data.Kind

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.HigherEff

data HUnion ops1 ops2
data LUnion ops1 ops2 inOps

data HUnionOps ops1 ops2
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)

data LUnionOps ops1 ops2
  inOps
  (eff :: Type -> Type)

instance EffOps (LUnion ops1 ops2 inOps) where
  type Operation (LUnion ops1 ops2 inOps) =
    LUnionOps ops1 ops2 inOps

instance EffFunctor (LUnionOps ops1 ops2 inOps) where
  effmap _ _ = undefined

instance
  ( HigherEff ops1
  , HigherEff ops2
  )
  => HigherEff (HUnion ops1 ops2)
   where
    type LowerEff (HUnion ops1 ops2) inOps =
      LUnion ops1 ops2 inOps

    type HOperation (HUnion ops1 ops2) eff =
      HUnionOps ops1 ops2 eff

    type TransformEff (HUnion ops1 ops2) eff =
      TransformEff ops1 (TransformEff ops2 eff)

    packHigherOps = undefined
    higherOps = undefined
    liftHigherOps = undefined

    transformEff = undefined
    liftWeaver = undefined
