{-# LANGUAGE UndecidableInstances #-}

module Casimir.Base.Union
  ( Union
  , UnionOps (..)
  , (∪)
  , type (∪)
  , leftOps
  , rightOps
  , pattern UnionOps
  )
where

import Data.Kind

import Casimir.Base.EffOps
import Casimir.Base.EffFunctor

-- | Combines two mect operations into a new mect operation. Union can
-- be used multiple times to combine multiple mect operations into one.

data Union ops1 ops2

newtype UnionOps ops1 ops2 (m :: Type -> Type)
  = MkUnionOps
    { unUnionOps :: ( ops1 m, ops2 m ) }

{-# COMPLETE UnionOps #-}
pattern UnionOps ops1 ops2 = MkUnionOps (ops1, ops2)

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
ops1 ∪ ops2 = UnionOps ops1 ops2

instance
  (EffOps ops1, EffOps ops2)
  => EffOps (Union ops1 ops2)
  where
    type Operation (Union ops1 ops2) =
      UnionOps (Operation ops1) (Operation ops2)

instance {-# INCOHERENT #-}
  ( EffFunctor lift ops1
  , EffFunctor lift ops2
  )
  => EffFunctor lift (UnionOps ops1 ops2)
  where
    effmap f (UnionOps x y)
      = UnionOps (effmap f x) (effmap f y)

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
