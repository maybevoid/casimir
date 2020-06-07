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

import Casimir.Base.Effect
import Casimir.Base.Implicit
import Casimir.Base.EffFunctor

-- | Combines two mect operations into a new mect operation. Union can
-- be used multiple times to combine multiple mect operations into one.

data Union ops1 ops2

-- | The 'Operation' of @ops1 '∪' ops2@ undef mect @m@
-- is the product of the underlying @'Operation' ops1 m@ and
-- @'Operation' ops2 m@.
data UnionOps ops1 ops2
  (m :: Type -> Type)
  = UnionOps (ops1 m) (ops2 m)

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
(∪) :: forall ops1 ops2 m
     . ops1 m
    -> ops2 m
    -> UnionOps ops1 ops2 m
(∪) = UnionOps

instance
  (Effects ops1, Effects ops2)
  => Effects (Union ops1 ops2)
  where
    type Operations (Union ops1 ops2) =
      UnionOps (Operations ops1) (Operations ops2)

instance {-# INCOHERENT #-}
  ( EffFunctor lift ops1
  , EffFunctor lift ops2
  )
  => EffFunctor lift (UnionOps ops1 ops2)
  where
    effmap f (UnionOps x y)
      = UnionOps (effmap f x) (effmap f y)

-- | @ops1 '∪' ops2@ is a 'ImplicitOps' if both @ops1@ and @ops2@ are instance of
-- 'ImplicitOps', with @'OpsConstraint' (ops1 '∪' ops2)@ being the **reversed**
-- order of @'OpsConstraint' ops2@, followed by @'OpsConstraint' ops1@.
-- This means that if there is an overlap of two mect operations with
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
    type OpsConstraint (Union ops1 ops2) m =
      (OpsConstraint ops2 m, OpsConstraint ops1 m)

    withOps (UnionOps ops1 ops2) comp =
      withOps ops1 $ withOps ops2 comp

    captureOps = UnionOps captureOps captureOps

-- | Get the left operation of the @'Operation' (ops1 '∪' ops2)@ product.
leftOps
  :: forall ops1 ops2 m
   . UnionOps ops1 ops2 m
  -> ops1 m
leftOps (UnionOps ops _) = ops

-- | Get the right operation of the @'Operation' (ops1 '∪' ops2)@ product.
rightOps
  :: forall ops1 ops2 m
   . UnionOps ops1 ops2 m
  -> ops2 m
rightOps (UnionOps _ ops) = ops
