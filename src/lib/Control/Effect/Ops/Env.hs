
module Control.Effect.Ops.Env
  ( EnvOps (..)
  , EnvModel (..)
  , EnvEff
  , ask
  , freeEnvOps
  )
where

import Control.Natural (type (~>))
import Control.Monad.Free (Free, liftF)

import Control.Effect.Class
  ( EffFunctor (..)
  , FreeEff (..)
  , EffOps (..)
  , liftEff
  )

data EnvOps a eff = EnvOps {
  askOp :: eff a
}

data EnvModel env r =
  AskOp (env -> r)

type EnvEff a eff = (?envOps :: EnvOps a eff)

instance EffFunctor (EnvOps a) where
  effmap f envOps = EnvOps {
    askOp = liftEff f $ askOp envOps
  }

instance Functor (EnvModel r) where
  fmap f (AskOp cont) = AskOp $ fmap f cont

instance FreeEff (EnvOps a) where
  type FreeModel (EnvOps a) = EnvModel a

  freeModel = freeEnvOps

instance EffOps (EnvOps a) where
  type EffConstraint (EnvOps a) eff = (EnvEff a eff)

  bindConstraint envOps comp = let ?envOps = envOps in comp

ask :: forall a eff . (EnvEff a eff) => eff a
ask = askOp ?envOps

freeEnvOps
  :: forall a f .
  (Functor f)
  => EnvModel a ~> f
  -> EnvOps a (Free f)
freeEnvOps liftModel = EnvOps {
  askOp = liftF $ liftModel $ AskOp id
}
