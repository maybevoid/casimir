
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
    type WrapComp (UnionOps ops1 ops2) h
      = UnionOps (WrapComp ops1 h) (WrapComp ops2 h)

    type WrapConstraint (UnionOps ops1 ops2) h
      = ( EffFunctor (WrapComp ops1 h)
        , EffFunctor (WrapComp ops2 h)
        , WrapConstraint ops1 h
        , WrapConstraint ops2 h
        )

    effmap f (UnionOps x y)
      = UnionOps (effmap f x) (effmap f y)

    wrapEff
      :: forall h eff1 eff2 .
      ( Effect eff1
      , Effect eff2
      , EffFunctor (WrapComp ops1 h)
      , EffFunctor (WrapComp ops2 h)
      , WrapConstraint ops1 h
      , WrapConstraint ops2 h
      , EffFunctor (WrapComp (UnionOps ops1 ops2) h)
      )
      => (forall a . eff1 a -> eff2 (h a))
      -> (UnionOps ops1 ops2) eff1
      -> UnionOps (WrapComp ops1 h) (WrapComp ops2 h) eff2
    wrapEff f (UnionOps x y)
      = UnionOps (wrapEff f x) (wrapEff f y)

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