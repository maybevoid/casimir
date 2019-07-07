{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Reflect.State
where

import Control.Monad.State.Class (MonadState  (..))

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Cast
import Control.Effect.Implicit.Ops.State

import Control.Effect.Implicit.Reflect.Reflect

newtype ReflectStateM ops eff a =
  ReflectStateM (eff a)
  deriving (Functor, Applicative, Monad)

instance
  ( Effect eff
  , ReifiesOps (StateEff s) eff
  )
  => MonadState s (ReflectStateM (StateEff s) eff)
  where
    get = ReflectStateM $ getOp $
      reflectOps @(StateEff s)

    put s = ReflectStateM $ putOp
      (reflectOps @(StateEff s))
      s

instance
  ReflectOps (StateEff s)
  where
    type ReflectM (StateEff s) eff
      = ReflectStateM (StateEff s) eff

    type OpsClass (StateEff s) eff
      = MonadState s eff

castStateClass
  :: forall eff s
   . (Effect eff)
  => Cast (MonadState s (ReflectM (StateEff s) eff))
  -> Cast (MonadState s eff)
castStateClass = castClass