{-# LANGUAGE DeriveFunctor #-}

module Control.Effect.Ops.Decide

where

import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT, liftF)

import Control.Effect.Class
import Control.Effect.Dynamic

data DecideOps s eff = DecideOps {
  decideOp :: eff s
}

data DecideModel s a = DecideOp (s -> a)
  deriving (Functor)

type DecideEff s eff = (?decideOps :: DecideOps s eff)

instance EffFunctor (DecideOps s) where
  effmap f decideOps = DecideOps {
    decideOp = liftEff f $ decideOp decideOps
  }

instance FreeEff (DecideOps s) where
  type FreeModel (DecideOps s) = DecideModel s

  freeModel = freeDecideOps

instance EffOps (DecideOps s) where
  type EffConstraint (DecideOps s) eff = DecideEff s eff

  bindConstraint decideOps comp = let ?decideOps = decideOps in comp

instance DynamicOps (DecideOps s) where
  dynamicOps = dynamicDecideOps

decide :: forall a eff .
  (DecideEff a eff)
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