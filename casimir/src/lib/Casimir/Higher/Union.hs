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

import qualified Casimir.Base as Base

data HUnionOps ops1 ops2
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  = HUnionOps
     (ops1 eff1 eff2)
     (ops2 eff1 eff2)

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
  ( Effect eff
  , EffFunctor (ops1 eff)
  , EffFunctor (ops2 eff)
  )
  => EffFunctor (HUnionOps ops1 ops2 eff)
  where
    effmap lifter (HUnionOps ops1 ops2) =
      HUnionOps
        (Base.effmap lifter ops1)
        (Base.effmap lifter ops2)

instance
  ( HigherEffFunctor ops1
  , HigherEffFunctor ops2
  )
  => HigherEffFunctor (HUnionOps ops1 ops2)
   where
    invEffmap lifter contraLift (HUnionOps ops1 ops2) =
      HUnionOps
        (invEffmap lifter contraLift ops1)
        (invEffmap lifter contraLift ops2)

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
