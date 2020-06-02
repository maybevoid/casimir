{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Casimir.Ops.State.Transform
where

import Data.Kind
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.State.Class (MonadState  (..))
import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import Casimir.Base
import Casimir.MonadOps
import Casimir.Computation

import qualified Casimir.Base as Base

import Casimir.Ops.State.Default
  ( StateEff
  , StateOps(..)
  , pattern StateOps
  , getOp
  , putOp
  )

import Casimir.Ops.State.Lift

data UseStateLift
  (lift :: (Type -> Type) -> (Type -> Type) -> Type)
  s t

type UseState = UseStateLift Lift
type UseHigherState = UseStateLift HigherLift

instance
  ( HasOps t )
  => HasOps (UseStateLift lift s t) where
    type SupportedOps (UseStateLift lift s t) = StateEff s ∪ SupportedOps t

instance
  ( MonadOps t
  , LiftMonoid lift
  , EffFunctor lift (Operation (SupportedOps t))
  , FreeLift (StateEff s) lift (OpsMonad t) (StateT s (OpsMonad t))
  )
  => MonadOps (UseStateLift lift s t) where
    type OpsMonad (UseStateLift lift s t) = StateT s (OpsMonad t)

    monadOps = stateTOps ∪
      effmap
        (freeLift @(StateEff s) @lift)
        (monadOps @t)

instance
  ( LiftMonadOps t
  , MonadOps (UseStateLift lift s t)
  )
  => LiftMonadOps (UseStateLift lift s t) where
    type BaseMonad (UseStateLift lift s t) = BaseMonad t

    liftBase = liftStateT . liftBase @t

instance
  ( ContraLiftMonadOps t
  , MonadOps (UseStateLift lift s t)
  )
  => ContraLiftMonadOps (UseStateLift lift s t) where
    contraLiftBase = joinContraLift
      (contraLiftBase @t)
      (stateTContraLift @(OpsMonad t) @s)

instance
  (Monad m)
  => FreeLift (StateEff s) Lift m (StateT s m) where
    freeLift = Lift liftStateT

instance
  (Monad m)
  => FreeLift (StateEff s) HigherLift m (StateT s m) where
    freeLift = HigherLift liftStateT stateTContraLift

stateTOps
  :: forall m s
   . (Monad m)
  => StateOps s (StateT s m)
stateTOps = StateOps {
  getOp = get,
  putOp = put
}

monadStateOps
  :: forall m s
   . (Monad m, MonadState s m)
  => StateOps s m
monadStateOps = StateOps {
  getOp = get,
  putOp = put
}

withStateTAndOps
  :: forall ops s r m .
  ( EffOps ops
  , EffFunctor Lift (Operation ops)
  , Monad m
  )
  => s
  -> Base.Operation ops m
  -> (Base.Operation (StateEff s ∪ ops) (StateT s m)
      -> StateT s m r)
  -> m r
withStateTAndOps i ops1 comp1 = evalStateT comp2 i
 where
  comp2 :: StateT s m r
  comp2 = comp1 ops2

  ops2 :: Base.Operation (StateEff s ∪ ops) (StateT s m)
  ops2 = stateTOps ∪ (effmap (Lift lift) ops1)

{-# INLINE stateTPipeline #-}
stateTPipeline
  :: forall s m1 comp .
  (Monad m1, EffFunctor Lift comp)
  => s
  -> SimplePipeline Lift NoEff (StateEff s) comp m1
stateTPipeline i = transformePipeline $ genericComputation handler
 where
  {-# INLINE handler #-}
  handler :: forall m
    . (Monad m)
    => TransformerHandler (StateT s) (StateEff s) m
  handler = TransformerHandler stateTOps stateTLift $ Lift $
    \comp -> evalStateT comp i
