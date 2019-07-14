{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.Union
where

import Data.Kind

import Control.Effect.Implicit.Higher.HigherOps

data HUnion ops1 ops2

data HUnionOps ops1 ops2
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  = HUnionOps
     (ops1 eff1 eff2)
     (ops2 eff1 eff2)

instance
  ( HigherOps ops1
  , HigherOps ops2
  )
  => HigherOps (HUnion ops1 ops2)
   where
    type HOperation (HUnion ops1 ops2)  =
      HUnionOps (HOperation ops1) (HOperation ops2)

instance
  (HigherEffFunctor ops1, HigherEffFunctor ops2)
  => HigherEffFunctor (HUnionOps ops1 ops2) where
    invEffmap lifter contraLifter (HUnionOps ops1 ops2) =
      HUnionOps
        (invEffmap lifter contraLifter ops1)
        (invEffmap lifter contraLifter ops2)
