
module Control.Effect.Base.Union

where

import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT)

import Control.Effect.Base.Effect
import Control.Effect.Base.EffOps
import Control.Effect.Base.FreeEff
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

instance (Functor f, Functor g)
  => Functor (UnionModel f g)
  where
    fmap f (LeftModel x) = LeftModel $ fmap f x
    fmap f (RightModel x) = RightModel $ fmap f x

instance
  ( EffFunctor f
  , EffFunctor g
  )
  => EffFunctor (UnionOps f g)
  where
    -- type WrapComp (UnionOps f g) h = UnionOps (WrapComp f h) (WrapComp g h)

    effmap f (UnionOps x y)
      = UnionOps (effmap f x) (effmap f y)

    -- wrapVal
    --   :: forall h eff .
    --   (Effect eff, EffFunctor (WrapComp f h), EffFunctor (WrapComp g h))
    -- wrapVal f (UnionOps x y)
    --   = UnionOps (wrapVal f x) (wrapVal f y)

instance (EffOps f, EffOps g) => FreeEff (Union f g) where
  type Operation (Union f g) = UnionOps (Operation f) (Operation g)
  type CoOperation (Union f g) = UnionModel (CoOperation f) (CoOperation g)

  freeMonad = freeUnionOps

instance (EffOps f, EffOps g) => EffOps (Union f g) where
  -- reverse the order as the left most constraint
  -- gets precedence if there is an overlap
  type OpsConstraint (Union f g) eff =
    (OpsConstraint g eff, OpsConstraint f eff)

  bindConstraint (UnionOps x y) comp =
    bindConstraint x $ bindConstraint y comp

  captureOps = UnionOps captureOps captureOps

freeUnionOps
  :: forall ops1 ops2 f eff.
  ( EffOps ops1
  , EffOps ops2
  , Functor f
  , Effect eff
  )
  => UnionModel (CoOperation ops1) (CoOperation ops2) ~> f
  -> UnionOps (Operation ops1) (Operation ops2) (FreeT f eff)
freeUnionOps liftModel = UnionOps ops1 ops2
  where
    ops1 :: Operation ops1 (FreeT f eff)
    ops1 = freeMonad (liftModel . LeftModel)

    ops2 :: Operation ops2 (FreeT f eff)
    ops2 = freeMonad (liftModel . RightModel)