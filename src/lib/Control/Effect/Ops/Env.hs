{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Ops.Env
  ( EnvEff
  , EnvOps (..)
  , EnvCoOps (..)
  , EnvConstraint
  , ask
  , mkEnvOps
  , mkEnvHandler
  )
where

import Control.Effect.Base
  ( Effect
  , EffFunctor (..)
  , FreeOps (..)
  , EffOps (..)
  , UnionOps (..)
  , Normalizable (..)
  )

import Control.Effect.Computation
  (BaseHandler, baseHandler)

data EnvEff e where

data EnvOps e eff = EnvOps {
  askOp :: eff e
}

data EnvCoOps env r =
  AskOp (env -> r)

type EnvConstraint e eff = (?envOps :: EnvOps e eff)

instance EffFunctor (EnvOps e) where
  effmap liftEff envOps = EnvOps {
    askOp = liftEff $ askOp envOps
  }

instance Functor (EnvCoOps e) where
  fmap f (AskOp cont) = AskOp $ fmap f cont

instance FreeOps (EnvEff e) where
  type Operation (EnvEff e) = EnvOps e
  type CoOperation (EnvEff e) = EnvCoOps e

  mkFreeOps liftCoOps = EnvOps {
    askOp = liftCoOps $ AskOp id
  }

instance EffOps (EnvEff e) where
  type OpsConstraint (EnvEff e) eff = (EnvConstraint e eff)

  bindConstraint envOps comp = let ?envOps = envOps in comp

  captureOps = ?envOps

instance Normalizable (EnvEff e) where
  unionOps = UnionOps

ask :: forall e eff . (EnvConstraint e eff) => eff e
ask = askOp ?envOps

mkEnvOps :: forall e eff . (Effect eff) => e -> EnvOps e eff
mkEnvOps x = EnvOps {
  askOp = return x
}

mkEnvHandler
  :: forall e eff .
  (Effect eff)
  => e
  -> BaseHandler (EnvEff e) eff
mkEnvHandler = baseHandler . mkEnvOps
