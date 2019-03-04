{-# LANGUAGE DeriveFunctor #-}

module Control.Effect.Ops.Decide

where

import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT, liftF)

import Control.Effect.Base
import Control.Effect.Dynamic

data DecideEff s where

data DecideOps s eff = DecideOps {
  decideOp :: eff s
}

data DecideModel s a = DecideOp (s -> a)
  deriving (Functor)

type DecideConstraint s eff = (?decideOps :: DecideOps s eff)

instance EffFunctor (DecideOps s) where
  effmap liftEff decideOps = DecideOps {
    decideOp = liftEff $ decideOp decideOps
  }

instance FreeEff (DecideEff s) where
  type Operation (DecideEff s) = DecideOps s
  type CoOperation (DecideEff s) = DecideModel s

  freeMonad = freeDecideOps

instance EffOps (DecideEff s) where
  type OpsConstraint (DecideEff s) eff = DecideConstraint s eff

  bindConstraint decideOps comp = let ?decideOps = decideOps in comp

instance DynamicOps (DecideEff s) where
  dynamicOps = dynamicDecideOps

decide :: forall a eff .
  (DecideConstraint a eff)
  => eff a
decide = decideOp ?decideOps

freeDecideOps
  :: forall a f eff .
  (Functor f, Effect eff)
  => DecideModel a ~> f
  -> DecideOps a (FreeT f eff)
freeDecideOps liftModel = DecideOps {
  decideOp = liftF $ liftModel $ DecideOp id
}

dynamicDecideOps
  :: forall s eff .
  (Effect eff)
  => DecideOps s (DynamicEff (DecideModel s) eff)
dynamicDecideOps = DecideOps {
  decideOp = liftOps $ (DecideOp return)
}