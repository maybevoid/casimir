
module Control.Effect.Implicit.Ops.State
  ( StateEff
  , StateOps (..)
  , StateCoOp (..)
  , FreerStateCoOp (..)
  , get
  , put
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free
import Control.Effect.Implicit.Freer

data StateEff s where

data StateOps s eff = StateOps {
  getOp :: eff s,
  putOp :: s -> eff ()
}

data StateCoOp s a =
    GetOp (s -> a)
  | PutOp s (() -> a)

data FreerStateCoOp s a where
  GetOp' :: FreerStateCoOp s s
  PutOp' :: s -> FreerStateCoOp s ()

instance EffOps (StateEff s) where
  type Operation (StateEff s) = StateOps s

instance EffCoOp (StateEff s) where
  type CoOperation (StateEff s) = StateCoOp s

instance FreerEffCoOp (StateEff s) where
  type FreerCoOp (StateEff s) = FreerStateCoOp s

instance Functor (StateCoOp s) where
  fmap f (GetOp cont) = GetOp $ fmap f cont
  fmap f (PutOp s cont) = PutOp s $ fmap f cont

instance EffFunctor (StateOps a) where
  effmap lifter stateOps = StateOps {
    getOp = lifter $ getOp stateOps,
    putOp = lifter . putOp stateOps
  }

instance FreeOps (StateEff s) where
  mkFreeOps liftCoOp = StateOps {
    getOp = liftCoOp $ GetOp id,
    putOp = \x -> liftCoOp $ PutOp x id
  }

instance FreerOps (StateEff s) where
  mkFreerOps liftCoOp = StateOps {
    getOp = liftCoOp $ GetOp',
    putOp = \x -> liftCoOp $ PutOp' x
  }

instance ImplicitOps (StateEff s) where
  type OpsConstraint (StateEff s) eff =
    (?_Control_Effect_Implicit_Ops_State_stateOps :: StateOps s eff)

  {-# INLINE withOps #-}
  withOps stateOps comp =
    let
      ?_Control_Effect_Implicit_Ops_State_stateOps =
        stateOps in comp

  {-# INLINE captureOps #-}
  captureOps =
    ?_Control_Effect_Implicit_Ops_State_stateOps

{-# INLINE get #-}
get :: forall s . Eff (StateEff s) s
get = getOp captureOps

{-# INLINE put #-}
put :: forall s . s -> Eff (StateEff s) ()
put = putOp captureOps
