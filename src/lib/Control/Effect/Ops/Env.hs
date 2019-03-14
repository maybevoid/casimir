{-# LANGUAGE FlexibleInstances #-}

module Control.Effect.Ops.Env
  ( EnvEff
  , EnvOps (..)
  , EnvModel (..)
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

data EnvEff a where

data EnvOps a eff = EnvOps {
  askOp :: eff a
}

data EnvModel env r =
  AskOp (env -> r)

type EnvConstraint a eff = (?envOps :: EnvOps a eff)

instance EffFunctor (EnvOps a) where
  effmap liftEff envOps = EnvOps {
    askOp = liftEff $ askOp envOps
  }

instance Functor (EnvModel r) where
  fmap f (AskOp cont) = AskOp $ fmap f cont

instance FreeOps (EnvEff a) where
  type Operation (EnvEff a) = EnvOps a
  type CoOperation (EnvEff a) = EnvModel a

  mkFreeOps liftCoOps = EnvOps {
    askOp = liftCoOps $ AskOp id
  }

instance EffOps (EnvEff a) where
  type OpsConstraint (EnvEff a) eff = (EnvConstraint a eff)

  bindConstraint envOps comp = let ?envOps = envOps in comp

  captureOps = ?envOps

instance Normalizable (EnvEff a) where
  unionOps = UnionOps

ask :: forall a eff . (EnvConstraint a eff) => eff a
ask = askOp ?envOps

mkEnvOps :: forall a eff . (Effect eff) => a -> EnvOps a eff
mkEnvOps x = EnvOps {
  askOp = return x
}

mkEnvHandler
  :: forall a eff .
  (Effect eff)
  => a
  -> BaseHandler (EnvEff a) eff
mkEnvHandler = baseHandler . mkEnvOps
