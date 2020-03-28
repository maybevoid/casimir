
module Control.Effect.Implicit.Ops.State.Transform
where

import Data.Tuple (swap)
import Control.Monad.State.Class (MonadState  (..))
import Control.Monad.Trans.State.Strict (StateT, evalStateT, runStateT)

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation
import Control.Effect.Implicit.Higher
  (HigherLiftEff (..))

import qualified Control.Effect.Implicit.Base as Base

import Control.Effect.Implicit.Ops.State.Base
  (StateEff, StateOps(..))

data UseState s t

data UseHigherState s t

instance
  ( BaseMonadOps t )
  => MonadOps (UseState s t) where
    type HasOps (UseState s t) = StateEff s ∪ HasOps t

    type OpsMonad (UseState s t) = StateT s (OpsMonad t)

    monadOps = stateTOps ∪ effmap liftStateT (monadOps @t)

instance
  ( HigherMonadOps t )
  => MonadOps (UseHigherState s t) where
    type HasOps (UseHigherState s t) = StateEff s ∪ HasOps t

    type OpsMonad (UseHigherState s t) = StateT s (OpsMonad t)

    monadOps = stateTOps
      ∪ invEffmap liftStateT stateTContraLift (monadOps @t)


liftStateT
  :: forall s eff a . (Effect eff)
  => eff a
  -> StateT s eff a
liftStateT = lift

stateTLiftEff
  :: forall s eff . (Effect eff)
  => LiftEff eff (StateT s eff)
stateTLiftEff = mkLiftEff liftStateT

stateTHigherLiftEff
  :: forall s eff . (Effect eff)
  => HigherLiftEff eff (StateT s eff)
stateTHigherLiftEff =
  HigherLiftEff liftStateT stateTContraLift

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

stateTContraLift
  :: forall eff s
   . (Effect eff)
  => ContraLift eff (StateT s eff)
stateTContraLift = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . ((forall x . StateT s eff x -> eff (s, x))
        -> eff (s, a))
    -> StateT s eff a
  contraLift1 cont1 = do
    s1 <- get
    let
      contraLift2 :: forall x . StateT s eff x -> eff (s, x)
      contraLift2 comp = swap <$> runStateT comp s1
    (s2, x) <- lift $ cont1 contraLift2
    put s2
    return x

withStateTAndOps
  :: forall ops s r eff .
  ( BaseOps ops
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
  -> SimplePipeline LiftEff NoEff (StateEff s) comp eff1
stateTPipeline i = transformePipeline $ genericComputation handler
 where
  {-# INLINE handler #-}
  handler :: forall eff
    . (Effect eff)
    => TransformerHandler (StateT s) (StateEff s) eff
  handler = TransformerHandler stateTOps stateTLiftEff $ mkLiftEff $
    \comp -> evalStateT comp i
