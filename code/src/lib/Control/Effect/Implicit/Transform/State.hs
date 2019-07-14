
module Control.Effect.Implicit.Transform.State
where

import Control.Monad.State.Class (MonadState  (..))
import Control.Monad.Trans.State.Strict (StateT, evalStateT, runStateT)

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation
import Control.Effect.Implicit.Higher

import Control.Effect.Implicit.Ops.Env
import Control.Effect.Implicit.Ops.State
  (StateEff, StateOps(..))

liftStateT
  :: forall s eff a . (Effect eff)
  => eff a
  -> StateT s eff a
liftStateT = lift

stateTLiftEff
  :: forall s eff . (Effect eff)
  => LiftEff eff (StateT s eff)
stateTLiftEff = mkLiftEff liftStateT

stateTOps
  :: forall eff s
   . (Effect eff, MonadState s eff)
  => StateOps s eff
stateTOps = StateOps {
  getOp = get,
  putOp = put
}

stateTWeaver
  :: forall eff s
   . (Effect eff)
  => Weaver eff (StateT s eff)
stateTWeaver = Weaver weaver1
 where
  weaver1 :: StateT s eff (WeaverOps eff (StateT s eff) ((,) s))
  weaver1 = do
    s <- get
    return $ weaver2 s

  weaver2 :: s -> WeaverOps eff (StateT s eff) ((,) s)
  weaver2 s1 = WeaverOps suspend resume
   where
    suspend :: forall a . StateT s eff a -> eff (s, a)
    suspend comp = do
      (x, s2) <- runStateT comp s1
      return $ (s2, x)

    resume :: forall a . (s, a) -> StateT s eff a
    resume (s2, x) = do
      put s2
      return x

stateTHandler
  :: forall eff s .
  (Effect eff, MonadState s eff)
  => OpsHandler NoEff (StateEff s) eff
stateTHandler = opsHandlerComp $
  \lifter -> applyEffmap lifter stateTOps

{-# INLINE stateTPipeline #-}
stateTPipeline
  :: forall s eff1 comp .
  (Effect eff1, EffFunctor comp)
  => s
  -> SimplePipeline NoEff (StateEff s) comp eff1
stateTPipeline i = transformePipeline $ genericComputation handler
 where
  {-# INLINE handler #-}
  handler :: forall eff
    . (Effect eff)
    => TransformerHandler (StateT s) (StateEff s) eff
  handler = TransformerHandler stateTOps stateTLiftEff $ mkLiftEff $
    \comp -> evalStateT comp i

{-# INLINE stateTToEnvEffPipeline #-}
stateTToEnvEffPipeline
  :: forall s eff1 comp .
  (Effect eff1, EffFunctor comp)
  => SimplePipeline (EnvEff s) (StateEff s) comp eff1
stateTToEnvEffPipeline = transformePipeline $ genericComputation handler
 where
  {-# INLINE handler #-}
  handler :: forall eff
   . (EffConstraint (EnvEff s) eff)
    => TransformerHandler (StateT s) (StateEff s) eff
  handler = TransformerHandler stateTOps stateTLiftEff $ mkLiftEff $
    \comp -> do
      i <- ask
      evalStateT comp i

withStateTAndOps
  :: forall ops s r eff .
  ( BaseOps ops
  , EffConstraint ops eff
  )
  => s
  -> ((EffConstraint (StateEff s ∪ ops) (StateT s eff))
      => StateT s eff r)
  -> eff r
withStateTAndOps i comp1 = evalStateT comp2 i
 where
  comp2 :: StateT s eff r
  comp2 = withOps ops comp1

  ops :: Operation (StateEff s ∪ ops) (StateT s eff)
  ops = stateTOps ∪ effmap lift captureOps
