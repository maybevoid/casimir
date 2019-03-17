
module Control.Effect.Transform.State
where

import Control.Monad.Trans.State.Strict

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Effect.Base
import Control.Effect.Computation

import Control.Effect.Ops.Env
import Control.Effect.Ops.State
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

stateTPipeline
  :: forall s eff1 comp .
  (Effect eff1, EffFunctor comp)
  => SimplePipeline (EnvEff s) (StateEff s) eff1 comp
stateTPipeline = transformerPipeline $ genericComputation handler
 where
  handler :: forall eff
   . (Effect eff, OpsConstraint (EnvEff s) eff)
    => TransformerHandler (StateT s) (StateEff s) eff
  handler = TransformerHandler stateTOps liftStateT $ mkLiftEff $
    \eff -> do
      i <- ask
      evalStateT eff i
