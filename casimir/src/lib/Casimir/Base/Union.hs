{-# LANGUAGE UndecidableInstances #-}

module Casimir.Base.Union
  ( Union
  , UnionOps (..)
  , (∪)
  , type (∪)
  , leftOps
  , rightOps
  )
where

import Data.Kind

import Casimir.Base.EffOps
import Casimir.Base.Implicit
import Casimir.Base.EffFunctor

-- | Combines two effect operations into a new effect operation. Union can
-- be used multiple times to combine multiple effect operations into one.

data Union ops1 ops2

-- | The 'Operation' of @ops1 '∪' ops2@ undef effect @eff@
-- is the product of the underlying @'Operation' ops1 eff@ and
-- @'Operation' ops2 eff@.
data UnionOps ops1 ops2
  (eff :: Type -> Type)
  = UnionOps (ops1 eff) (ops2 eff)

infixr 7 ∪
infixr 7 `Union`

-- | Right associative type operator alias for 'Union', e.g.
--
-- @
-- forall ops1 ops2 ops3 :: Type .
--   ops1 ∪ ops2 ∪ ops3 === Union ops1 (Union ops2 ops3)
-- @
type (∪) = Union

-- | Right associative term operator alias for 'Union', e.g.
--
-- @
-- forall ops1 ops2 ops3 .
--   ops1 ∪ ops2 ∪ ops3 === Union ops1 (Union ops2 ops3)
-- @
(∪) :: forall ops1 ops2 eff
     . ops1 eff
    -> ops2 eff
    -> UnionOps ops1 ops2 eff
(∪) = UnionOps

instance
  (EffOps ops1, EffOps ops2)
  => EffOps (Union ops1 ops2)
  where
    type Operation (Union ops1 ops2) =
      UnionOps (Operation ops1) (Operation ops2)

instance
  ( EffFunctor ops1
  , EffFunctor ops2
  )
  => EffFunctor (UnionOps ops1 ops2)
  where
    effmap f (UnionOps x y)
      = UnionOps (effmap f x) (effmap f y)

instance
  (HigherEffFunctor ops1, HigherEffFunctor ops2)
  => HigherEffFunctor (UnionOps ops1 ops2) where
    invEffmap lift contraLift (UnionOps ops1 ops2) =
      UnionOps
        (invEffmap lift contraLift ops1)
        (invEffmap lift contraLift ops2)

-- | @ops1 '∪' ops2@ is a 'ImplicitOps' if both @ops1@ and @ops2@ are instance of
-- 'ImplicitOps', with @'OpsConstraint' (ops1 '∪' ops2)@ being the **reversed**
-- order of @'OpsConstraint' ops2@, followed by @'OpsConstraint' ops1@.
-- This means that if there is an overlap of two effect operations with
-- the same implicit parameter label, the one defined at the right side
-- of '∪' gets selected by GHC. For plain implicit parameter constraints,
-- when there is an overlap, the left most implicit parameter is used,
-- and hence the reason the constraint order have to be reversed.
instance
  (ImplicitOps ops1, ImplicitOps ops2)
  => ImplicitOps (Union ops1 ops2)
   where
    -- | Reverse the order as the left most constraint
    -- gets precedence if there is an overlap
    type OpsConstraint (Union ops1 ops2) eff =
      (OpsConstraint ops2 eff, OpsConstraint ops1 eff)

    withOps (UnionOps ops1 ops2) comp =
      withOps ops1 $ withOps ops2 comp

    captureOps = UnionOps captureOps captureOps

-- | Get the left operation of the @'Operation' (ops1 '∪' ops2)@ product.
leftOps
  :: forall ops1 ops2 eff
   . UnionOps ops1 ops2 eff
  -> ops1 eff
leftOps (UnionOps ops _) = ops

-- | Get the right operation of the @'Operation' (ops1 '∪' ops2)@ product.
rightOps
  :: forall ops1 ops2 eff
   . UnionOps ops1 ops2 eff
  -> ops2 eff
rightOps (UnionOps _ ops) = ops
