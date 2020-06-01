{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}

module Casimir.Higher.Union
where

import Data.Kind

import Casimir.Base
  ( Union
  , EffFunctor (..)
  )
import Casimir.Higher.Base
import Casimir.Higher.CoOp
import Casimir.Higher.Free
import Casimir.Higher.EffFunctor

data HUnionOps ops1 ops2
  (m1 :: Type -> Type)
  (m2 :: Type -> Type)
  = HUnionOps
     (ops1 m1 m2)
     (ops2 m1 m2)

data HUnionCoOp coop1 coop2
  (f :: Type -> Type)
  r
  = LeftOp (coop1 f r)
  | RightOp (coop2 f r)

instance
  ( EffOps ops1
  , EffOps ops2
  )
  => EffOps (Union ops1 ops2) where
    type Operation (Union ops1 ops2) =
      HUnionOps (Operation ops1) (Operation ops2)

instance
  ( EffCoOp ops1
  , EffCoOp ops2
  )
  => EffCoOp (Union ops1 ops2) where
    type CoOperation (Union ops1 ops2) =
      HUnionCoOp (CoOperation ops1) (CoOperation ops2)

instance
  ( Monad m
  , EffFunctor lift (ops1 m)
  , EffFunctor lift (ops2 m)
  )
  => EffFunctor lift (HUnionOps ops1 ops2 m)
  where
    mmap lifter (HUnionOps ops1 ops2) =
      HUnionOps
        (mmap lifter ops1)
        (mmap lifter ops2)

instance
  ( HigherEffFunctor lift ops1
  , HigherEffFunctor lift ops2
  )
  => HigherEffFunctor lift (HUnionOps ops1 ops2)
   where
    higherEffmap lift (HUnionOps ops1 ops2) =
      HUnionOps
        (higherEffmap lift ops1)
        (higherEffmap lift ops2)

instance
  ( Functor f
  , Functor (coop1 f)
  , Functor (coop2 f)
  )
  => Functor (HUnionCoOp coop1 coop2 f) where
    fmap f (LeftOp op) = LeftOp $ fmap f op
    fmap f (RightOp op) = RightOp $ fmap f op

instance
  ( CoOpFunctor coop1
  , CoOpFunctor coop2
  )
  => CoOpFunctor (HUnionCoOp coop1 coop2) where
    liftCoOp f (LeftOp op) = LeftOp $ liftCoOp f op
    liftCoOp f (RightOp op) = RightOp $ liftCoOp f op

instance
  ( FreeOps ops1
  , FreeOps ops2
  )
  => FreeOps (Union ops1 ops2) where
    mkFreeOps liftReturn = HUnionOps ops1 ops2
     where
      ops1 = mkFreeOps (liftReturn . LeftOp)
      ops2 = mkFreeOps (liftReturn . RightOp)
