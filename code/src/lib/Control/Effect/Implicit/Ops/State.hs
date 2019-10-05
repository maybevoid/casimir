
module Control.Effect.Implicit.Ops.State
  ( StateOps (..)
  , StateCoOp (..)
  , FreerStateCoOp (..)
  , get
  , put
  )
where

import Control.Implicit.Param
import Control.Effect.Implicit.Base

import qualified Control.Effect.Implicit.Free as Free
import qualified Control.Effect.Implicit.Freer as Freer

data StateTag

data StateOps s eff = StateOps {
  getOp :: eff s,
  putOp :: s -> eff ()
}

data StateCoOp s r =
    GetOp (s -> r)
  | PutOp s (() -> r)

data FreerStateCoOp s a where
  GetOp' :: FreerStateCoOp s s
  PutOp' :: s -> FreerStateCoOp s ()

instance Free.EffCoOp (StateOps s) where
  type CoOperation (StateOps s) = StateCoOp s

instance Freer.EffCoOp (StateOps s) where
  type CoOperation (StateOps s) = FreerStateCoOp s

instance Functor (StateCoOp s) where
  fmap f (GetOp cont) = GetOp $ fmap f cont
  fmap f (PutOp s cont) = PutOp s $ fmap f cont

instance EffFunctor (StateOps a) where
  effmap lifter stateOps = StateOps {
    getOp = lifter $ getOp stateOps,
    putOp = lifter . putOp stateOps
  }

instance Free.FreeOps (StateOps s) where
  mkFreeOps liftCoOp = StateOps {
    getOp = liftCoOp $ GetOp id,
    putOp = \x -> liftCoOp $ PutOp x id
  }

instance Freer.FreeOps (StateOps s) where
  mkFreeOps liftCoOp = StateOps {
    getOp = liftCoOp $ GetOp',
    putOp = \x -> liftCoOp $ PutOp' x
  }

instance ImplicitOps (StateOps s) where
  type OpsConstraint (StateOps s) eff =
    TaggedParam StateTag (StateOps s eff)

  withOps = withTag @StateTag
  captureOps = captureTag @StateTag

{-# INLINE get #-}
get :: forall s . Eff (StateOps s) s
get = getOp captureOps

{-# INLINE put #-}
put :: forall s . s -> Eff (StateOps s) ()
put = putOp captureOps
