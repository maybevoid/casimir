{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.Union
where

import Data.Kind

import Control.Effect.Implicit.Higher.HigherOps

data HUnion ops1 ops2

data HUnionOps ops1 ops2
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)

instance
  ( HigherOps ops1
  , HigherOps ops2
  )
  => HigherOps (HUnion ops1 ops2)
   where
    type HOperation (HUnion ops1 ops2)  =
      HUnionOps ops1 ops2

instance HigherOpsFunctor (HUnionOps ops1 ops2) where
    liftHigherOps = undefined
