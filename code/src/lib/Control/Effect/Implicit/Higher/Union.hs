{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.Union
where

import Data.Kind

import Control.Effect.Implicit.Base.Union (Union)
import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.CoOp
import Control.Effect.Implicit.Higher.Free
import Control.Effect.Implicit.Higher.Implicit
import Control.Effect.Implicit.Higher.EffFunctor

import qualified Control.Effect.Implicit.Base as Base

data UnionOps ops1 ops2
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  = UnionOps
     (ops1 eff1 eff2)
     (ops2 eff1 eff2)

data UnionCoOp coop1 coop2
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
      UnionOps (Operation ops1) (Operation ops2)

instance
  ( EffCoOp ops1
  , EffCoOp ops2
  )
  => EffCoOp (Union ops1 ops2) where
    type CoOperation (Union ops1 ops2) =
      UnionCoOp (CoOperation ops1) (CoOperation ops2)

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

instance
  (ImplicitOps ops1, ImplicitOps ops2)
  => ImplicitOps (Union ops1 ops2)
   where
    type OpsConstraint (Union ops1 ops2) eff1 eff2 =
      (OpsConstraint ops2 eff1 eff2, OpsConstraint ops1 eff1 eff2)

    withHigherOps (UnionOps ops1 ops2) comp =
      withHigherOps ops1 $ withHigherOps ops2 comp

    captureHigherOps = UnionOps captureHigherOps captureHigherOps

instance
  ( Functor f
  , Functor (coop1 f)
  , Functor (coop2 f)
  )
  => Functor (UnionCoOp coop1 coop2 f) where
    fmap f (LeftOp op) = LeftOp $ fmap f op
    fmap f (RightOp op) = RightOp $ fmap f op

instance
  ( CoOpFunctor coop1
  , CoOpFunctor coop2
  )
  => CoOpFunctor (UnionCoOp coop1 coop2) where
    liftCoOp f (LeftOp op) = LeftOp $ liftCoOp f op
    liftCoOp f (RightOp op) = RightOp $ liftCoOp f op

instance
  ( FreeOps ops1
  , FreeOps ops2
  )
  => FreeOps (Union ops1 ops2) where
    mkFreeOps liftReturn = UnionOps ops1 ops2
     where
      ops1 = mkFreeOps (liftReturn . LeftOp)
      ops2 = mkFreeOps (liftReturn . RightOp)
