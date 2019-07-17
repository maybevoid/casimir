
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

stateTContraLift
  :: forall eff s
   . (Effect eff)
  => ContraLiftEff eff (StateT s eff)
stateTContraLift = ContraLiftEff contraLift1
 where
  contraLift1
    :: StateT s eff (ContraLiftOps eff (StateT s eff) ((,) s))
  contraLift1 = do
    s <- get
    return $ contraLift2 s

  contraLift2 :: s -> ContraLiftOps eff (StateT s eff) ((,) s)
  contraLift2 s1 = ContraLiftOps suspend resume
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
  , Effect eff
  )
  => s
  -> Operation ops eff
  -> (Operation (StateEff s ∪ ops) (StateT s eff)
      -> StateT s eff r)
  -> eff r
withStateTAndOps i ops1 comp1 = evalStateT comp2 i
 where
  comp2 :: StateT s eff r
  comp2 = comp1 ops2

  ops2 :: Operation (StateEff s ∪ ops) (StateT s eff)
  ops2 = stateTOps ∪ (effmap lift ops1)
