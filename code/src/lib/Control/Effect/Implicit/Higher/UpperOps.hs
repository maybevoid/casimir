{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.UpperOps
where

import Data.Kind

import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.CoOp
import Control.Effect.Implicit.Higher.EffFunctor
import Control.Effect.Implicit.Higher.ContraLift

import qualified Control.Effect.Implicit.Base as Base
import qualified Control.Effect.Implicit.Free as Base

class
  ( Base.EffOps ops
  , EffOps ops
  , Operation ops ~ UpperOps (Base.Operation ops)
  )
  => HigherOps ops where

class
  ( Base.EffCoOp ops
  , EffCoOp ops
  , CoOperation ops ~ UpperCoOp (Base.CoOperation ops)
  )
  => HigherCoOp ops where

data UpperOps ops
  (inEff :: Type -> Type)
  (eff :: Type -> Type)
  = UpperOps
    { innerOps' :: ops inEff
    , outerOps' :: ops eff
    }

data UpperCoOp coop f r = UpperOp (coop (f r))

instance
  ( Effect eff
  , Base.EffFunctor ops
  )
  => Base.EffFunctor (UpperOps ops eff)
  where
    effmap
      :: forall eff1 eff2
       . (Effect eff1, Effect eff2)
      => (forall x. eff1 x -> eff2 x)
      -> UpperOps ops eff eff1
      -> UpperOps ops eff eff2
    effmap lifter (UpperOps ops1 ops2) =
      UpperOps ops1 (Base.effmap lifter ops2)

instance
  (Base.EffFunctor ops)
  => EffFunctor (UpperOps ops)
   where
    invEffmap
      :: forall eff1 eff2
       . (Effect eff1, Effect eff2)
      => (forall x. eff1 x -> eff2 x)
      -> ContraLift eff1 eff2
      -> UpperOps ops eff1 eff1
      -> UpperOps ops eff2 eff2
    invEffmap lifter _ (UpperOps ops1 ops2) =
      UpperOps (Base.effmap lifter ops1) (Base.effmap lifter ops2)

liftHigherOps
  :: forall ops eff
   . (Effect eff, HigherOps ops)
  => Base.Operation ops eff
  -> Operation ops eff eff
liftHigherOps ops = UpperOps ops ops