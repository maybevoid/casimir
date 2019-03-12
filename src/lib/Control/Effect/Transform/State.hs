
module Control.Effect.Transform.State
where

import Control.Monad.Trans.State.Strict

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Effect.Base
import Control.Effect.Computation

import Control.Effect.Ops.State
  (StateEff, StateOps(..))

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
stateTHandler = mkHandler lift $
  \liftEff -> StateOps {
    getOp = liftEff get,
    putOp = \x -> liftEff $ put x
  }

stateTPipeline
  :: forall s eff1 comp .
  (Effect eff1, EffFunctor comp)
  => s
  -> SimplePipeline NoEff (StateEff s) eff1 comp
stateTPipeline i = transformerPipeline $ genericComputation handler
 where
  handler :: forall eff . (Effect eff)
    => TransformerHandler (StateT s) (StateEff s) eff
  handler = TransformerHandler stateTOps lift $
    \eff -> evalStateT eff i
