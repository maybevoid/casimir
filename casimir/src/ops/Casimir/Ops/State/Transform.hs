{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE PolyKinds #-}
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

import Casimir.Ops.State.Base (StateOps(..))

import Casimir.Ops.State.Lift

data LiftState s

data UseStateLift
  (lift :: (Type -> Type) -> (Type -> Type) -> Type)
  (s :: Type)
  t

type UseState = UseStateLift Lift
type UseHigherState = UseStateLift HigherLift

instance
  ( HasOps t )
  => HasOps (UseStateLift lift s t) where
    type SupportedOps (UseStateLift lift s t) = Union (StateOps s) (SupportedOps t)

instance
  ( MonadOps t
  , LiftMonoid lift
  , EffFunctor lift (SupportedOps t)
  , FreeLift (LiftState s) lift (OpsMonad t) (StateT s (OpsMonad t))
  )
  => MonadOps (UseStateLift lift s t) where
    type OpsMonad (UseStateLift lift s t) = StateT s (OpsMonad t)

    monadOps = Union stateTOps $
      effmap
        (freeLift @(LiftState s) @lift)
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
  => FreeLift (LiftState s) Lift m (StateT s m) where
    freeLift = Lift liftStateT

instance
  (Monad m)
  => FreeLift (LiftState s) HigherLift m (StateT s m) where
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
  ( Effects ops
  , EffFunctor Lift ops
  , Monad m
  )
  => s
  -> ops m
  -> (Cons (StateOps s) ops (StateT s m)
      -> StateT s m r)
  -> m r
withStateTAndOps i ops1 comp1 = evalStateT comp2 i
 where
  comp2 :: StateT s m r
  comp2 = comp1 ops2

  ops2 :: Cons (StateOps s) ops (StateT s m)
  ops2 = Cons stateTOps (effmap (Lift lift) ops1)

{-# INLINE stateTPipeline #-}
stateTPipeline
  :: forall s m1 comp .
  (Monad m1, EffFunctor Lift comp)
  => s
  -> SimplePipeline Lift Nil (Multi '[StateOps s]) comp m1
stateTPipeline i = transformePipeline $ genericComputation @Nil handler
 where
  {-# INLINE handler #-}
  handler :: forall m
    . (Monad m)
    => TransformerHandler (StateT s) (Multi '[StateOps s]) m
  handler = TransformerHandler (Cons stateTOps Nil) stateTLift $ Lift $
    \comp -> evalStateT comp i
