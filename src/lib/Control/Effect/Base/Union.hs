
module Control.Effect.Base.Union

where

import Control.Effect.Base.Effect
import Control.Effect.Base.EffOps
import Control.Effect.Base.FreeOps
import Control.Effect.Base.EffFunctor

data Union f g where

data UnionOps
  (f :: (* -> *) -> *)
  (g :: (* -> *) -> *)
  (eff :: * -> *)
  = (EffFunctor f, EffFunctor g)
  => UnionOps (f eff) (g eff)

data UnionModel
  (f :: (* -> *))
  (g :: (* -> *))
  (a :: *)
  = LeftModel (f a)
  | RightModel (g a)

instance (Functor ops1, Functor ops2)
  => Functor (UnionModel ops1 ops2)
  where
    fmap f (LeftModel x) = LeftModel $ fmap f x
    fmap f (RightModel x) = RightModel $ fmap f x

instance
  ( EffFunctor ops1
  , EffFunctor ops2
  )
  => EffFunctor (UnionOps ops1 ops2)
  where
    effmap f (UnionOps x y)
      = UnionOps (effmap f x) (effmap f y)

instance (EffOps f, EffOps g) => FreeOps (Union f g) where
  type Operation (Union f g) = UnionOps (Operation f) (Operation g)
  type CoOperation (Union f g) = UnionModel (CoOperation f) (CoOperation g)

  mkFreeOps = mkFreeUnionOps

instance (EffOps f, EffOps g) => EffOps (Union f g) where
  -- reverse the order as the left most constraint
  -- gets precedence if there is an overlap
  type OpsConstraint (Union f g) eff =
    (OpsConstraint g eff, OpsConstraint f eff)

  bindConstraint (UnionOps x y) comp =
    bindConstraint x $ bindConstraint y comp

  captureOps = UnionOps captureOps captureOps

mkFreeUnionOps
  :: forall ops1 ops2 t eff.
  ( EffOps ops1
  , EffOps ops2
  , Effect eff
  , Effect (t eff)
  )
  => (forall a .
      UnionModel (CoOperation ops1) (CoOperation ops2) a
      -> t eff a)
  -> UnionOps (Operation ops1) (Operation ops2) (t eff)
mkFreeUnionOps liftReturn = UnionOps ops1 ops2
  where
    ops1 :: Operation ops1 (t eff)
    ops1 = mkFreeOps (liftReturn . LeftModel)

    ops2 :: Operation ops2 (t eff)
    ops2 = mkFreeOps (liftReturn . RightModel)