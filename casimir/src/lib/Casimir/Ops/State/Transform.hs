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

import Casimir.Ops.State.Base (StateEff, StateOps(..))

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
  , LiftOps lift
  , Liftable lift (SupportedOps t)
  , FreeLift (StateEff s) lift (OpsMonad t) (StateT s (OpsMonad t))
  )
  => MonadOps (UseStateLift lift s t) where
    type OpsMonad (UseStateLift lift s t) = StateT s (OpsMonad t)

    monadOps = stateTOps ∪
      applyLift
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
  (Effect eff)
  => FreeLift (StateEff s) Lift eff (StateT s eff) where
    freeLift = Lift liftStateT

instance
  (Effect eff)
  => FreeLift (StateEff s) HigherLift eff (StateT s eff) where
    freeLift = HigherLift liftStateT stateTContraLift

stateTOps
  :: forall eff s
   . (Effect eff)
  => StateOps s (StateT s eff)
stateTOps = StateOps {
  getOp = get,
  putOp = put
}

monadStateOps
  :: forall eff s
   . (Effect eff, MonadState s eff)
  => StateOps s eff
monadStateOps = StateOps {
  getOp = get,
  putOp = put
}

withStateTAndOps
  :: forall ops s r eff .
  ( EffOps ops
  , EffFunctor (Operation ops)
  , Effect eff
  )
  => s
  -> Base.Operation ops eff
  -> (Base.Operation (StateEff s ∪ ops) (StateT s eff)
      -> StateT s eff r)
  -> eff r
withStateTAndOps i ops1 comp1 = evalStateT comp2 i
 where
  comp2 :: StateT s eff r
  comp2 = comp1 ops2

  ops2 :: Base.Operation (StateEff s ∪ ops) (StateT s eff)
  ops2 = stateTOps ∪ (effmap lift ops1)

{-# INLINE stateTPipeline #-}
stateTPipeline
  :: forall s eff1 comp .
  (Effect eff1, Base.EffFunctor comp)
  => s
  -> SimplePipeline Lift NoEff (StateEff s) comp eff1
stateTPipeline i = transformePipeline $ genericComputation handler
 where
  {-# INLINE handler #-}
  handler :: forall eff
    . (Effect eff)
    => TransformerHandler (StateT s) (StateEff s) eff
  handler = TransformerHandler stateTOps stateTLift $ Lift $
    \comp -> evalStateT comp i
