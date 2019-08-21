
module Control.Effect.Implicit.Base.Union
  ( Union (..)
  , (∪)
  , type (∪)
  , leftOps
  , rightOps
  )
where

import Data.Kind
import Control.Effect.Implicit.Base.Implicit
import Control.Effect.Implicit.Base.EffFunctor

-- | Combines two effect operations into a new effect operation. Union can
-- be used multiple times to combine multiple effect operations into one.

-- | The 'Operation' of @ops1 '∪' ops2@ undef effect @eff@
-- is the product of the underlying @'Operation' ops1 eff@ and
-- @'Operation' ops2 eff@.
data Union ops1 ops2
  (eff :: Type -> Type)
  = Union (ops1 eff) (ops2 eff)

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
(∪) :: forall ops1 ops2 eff . ops1 eff -> ops2 eff -> Union ops1 ops2 eff
(∪) = Union

instance
  ( EffFunctor ops1
  , EffFunctor ops2
  )
  => EffFunctor (Union ops1 ops2)
  where
    effmap f (Union x y)
      = Union (effmap f x) (effmap f y)

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

    withOps (Union ops1 ops2) comp =
      withOps ops1 $ withOps ops2 comp

    captureOps = Union captureOps captureOps

-- | Get the left operation of the @'Operation' (ops1 '∪' ops2)@ product.
leftOps :: forall ops1 ops2 eff
   . Union ops1 ops2 eff
   -> ops1 eff
leftOps (Union ops _) = ops

-- | Get the right operation of the @'Operation' (ops1 '∪' ops2)@ product.
rightOps :: forall ops1 ops2 eff
   . Union ops1 ops2 eff
   -> ops2 eff
rightOps (Union _ ops) = ops
