
module Control.Effect.Implicit.Transform.State
where

import Control.Monad.Trans.State.Strict

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation

import Control.Effect.Implicit.Ops.Env
import Control.Effect.Implicit.Ops.State
  (StateEff, StateOps(..))

liftStateT
  :: forall s eff
   . (Effect eff)
  => LiftEff eff (StateT s eff)
liftStateT = mkLiftEff lift

stateTOps
  :: forall eff s .
  (Effect eff)
  => StateOps s (StateT s eff)
stateTOps = StateOps {
  getOp = get,
  putOp = put
}

stateTHandler
  :: forall eff s .
  (Effect eff)
  => Handler NoEff (StateEff s) (StateT s eff) eff
stateTHandler = mkHandler liftStateT $
  \lifter -> StateOps {
    getOp = liftEff lifter get,
    putOp = \x -> liftEff lifter $ put x
  }

{-# INLINE stateTPipeline #-}
stateTPipeline
  :: forall s eff1 comp .
  (Effect eff1, EffFunctor comp)
  => SimplePipeline (EnvEff s) (StateEff s) eff1 comp
stateTPipeline = transformerPipeline $ genericComputation handler
 where
  {-# INLINE handler #-}
  handler :: forall eff
   . (EffConstraint (EnvEff s) eff)
    => TransformerHandler (StateT s) (StateEff s) eff
  handler = TransformerHandler stateTOps liftStateT $ mkLiftEff $
    \eff -> do
      i <- ask
      evalStateT eff i
