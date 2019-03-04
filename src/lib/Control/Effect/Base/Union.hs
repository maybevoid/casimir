{-# LANGUAGE FlexibleContexts #-}

module Control.Effect.Base.Union

where

import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT)

import Control.Effect.Base.Effect
import Control.Effect.Base.EffOps
import Control.Effect.Base.FreeEff
import Control.Effect.Base.EffFunctor

data Union
  (f :: (* -> *) -> *)
  (g :: (* -> *) -> *)
  (eff :: * -> *)
  where
    Union :: f eff -> g eff -> Union f g eff

data UnionModel f g a =
  LeftModel (FreeModel f a)
  | RightModel (FreeModel g a)

instance (EffOps f, EffOps g) => Functor (UnionModel f g) where
  fmap f (LeftModel x) = LeftModel $ fmap f x
  fmap f (RightModel x) = RightModel $ fmap f x

instance (EffOps f, EffOps g) => EffFunctor (Union f g) where
  effmap f (Union x y) = Union (effmap f x) (effmap f y)

instance (EffOps f, EffOps g) => FreeEff (Union f g) where
  type FreeModel (Union f g) = UnionModel f g

  freeModel = freeUnionOps

instance (EffOps f, EffOps g) => EffOps (Union f g) where
  -- reverse the order as the left most constraint gets
  -- precedence if there is an overlap
  type EffConstraint (Union f g) eff = (EffConstraint g eff, EffConstraint f eff)

  bindConstraint (Union x y) comp =
    bindConstraint x $ bindConstraint y comp

freeUnionOps
  :: forall ops1 ops2 f eff.
  ( EffOps ops1
  , EffOps ops2
  , Functor f
  , Effect eff
  )
  => UnionModel ops1 ops2 ~> f
  -> Union ops1 ops2 (FreeT f eff)
freeUnionOps liftModel = Union ops1 ops2
  where
    ops1 :: ops1 (FreeT f eff)
    ops1 = freeModel (liftModel . LeftModel)

    ops2 :: ops2 (FreeT f eff)
    ops2 = freeModel (liftModel . RightModel)