
module Control.Effect.Transform.State
where

import Control.Monad.Trans.State.Strict

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Effect.Base
import Control.Effect.Computation

import Control.Effect.Ops.State
  (StateEff, StateOps(..))

stateTHandler
  :: forall eff s .
  (Effect eff)
  => Handler NoEff (StateEff s) (StateT s eff) eff
stateTHandler = mkHandler lift $
  \liftEff -> StateOps {
    getOp = liftEff get,
    putOp = \x -> liftEff $ put x
  }
