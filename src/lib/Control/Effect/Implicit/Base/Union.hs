
module Control.Effect.Implicit.Base.Union
where

import Data.Kind
import Control.Effect.Implicit.Base.EffOps
import Control.Effect.Implicit.Base.FreeOps
import Control.Effect.Implicit.Base.EffFunctor

data Union ops1 ops2

data UnionOps
  (ops1 :: (Type -> Type) -> Type)
  (ops2 :: (Type -> Type) -> Type)
  (eff :: Type -> Type)
  = UnionOps (ops1 eff) (ops2 eff)

infixr 7 ∪
type (∪) = Union
(∪) :: forall ops1 ops2 eff . ops1 eff -> ops2 eff -> UnionOps ops1 ops2 eff
(∪) = UnionOps

data UnionCoOp
  (ops1 :: (Type -> Type))
  (ops2 :: (Type -> Type))
  (a :: Type)
  = LeftCoOp (ops1 a)
  | RightCoOp (ops2 a)

instance (Functor ops1, Functor ops2)
  => Functor (UnionCoOp ops1 ops2)
  where
    fmap f (LeftCoOp x) = LeftCoOp $ fmap f x
    fmap f (RightCoOp x) = RightCoOp $ fmap f x

instance
  ( EffFunctor ops1
  , EffFunctor ops2
  )
  => EffFunctor (UnionOps ops1 ops2)
  where
    effmap f (UnionOps x y)
      = UnionOps (effmap f x) (effmap f y)

instance (FreeOps ops1, FreeOps ops2) => FreeOps (Union ops1 ops2) where
  type Operation (Union ops1 ops2) = UnionOps (Operation ops1) (Operation ops2)
  type CoOperation (Union ops1 ops2) = UnionCoOp (CoOperation ops1) (CoOperation ops2)

  mkFreeOps liftReturn = UnionOps ops1 ops2
   where
    ops1 = mkFreeOps (liftReturn . LeftCoOp)
    ops2 = mkFreeOps (liftReturn . RightCoOp)

instance (EffOps ops1, EffOps ops2) => EffOps (Union ops1 ops2) where
  -- reverse the order as the left most constraint
  -- gets precedence if there is an overlap
  type OpsConstraint (Union ops1 ops2) eff =
    (OpsConstraint ops2 eff, OpsConstraint ops1 eff)

  withOps (UnionOps ops1 ops2) comp =
    withOps ops1 $ withOps ops2 comp

  captureOps = UnionOps captureOps captureOps

leftOps :: forall ops1 ops2 eff
   . UnionOps ops1 ops2 eff
   -> ops1 eff
leftOps (UnionOps ops _) = ops

rightOps :: forall ops1 ops2 eff
   . UnionOps ops1 ops2 eff
   -> ops2 eff
rightOps (UnionOps _ ops) = ops