{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Free.Union
  ( UnionCoOp (..)
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free.EffCoOp
import Control.Effect.Implicit.Free.FreeOps

-- | The 'CoOperation' of @ops1 '∪' ops2@ under return type
-- @r@ is the sum of the underlying @'CoOperation' ops1 r@ and
-- @'CoOperation' ops2 r@.
data UnionCoOp ops1 ops2 r
  = LeftCoOp (ops1 r)
  | RightCoOp (ops2 r)

instance EffCoOp (Union ops1 ops2) where
  type CoOperation (Union ops1 ops2) =
    UnionCoOp (CoOperation ops1) (CoOperation ops2)

instance (Functor ops1, Functor ops2)
  => Functor (UnionCoOp ops1 ops2)
  where
    fmap f (LeftCoOp x) = LeftCoOp $ fmap f x
    fmap f (RightCoOp x) = RightCoOp $ fmap f x

-- | @ops1 '∪' ops2@ is a 'FreeOps' if both @ops1@ and @ops2@ are instance of
-- 'FreeOps', with @'Operation' (ops1 '∪' ops2)@ being the product of the
-- underlying 'Operation's and @'CoOperation' (ops1 '∪' ops2)@ being the
-- sum of the underlying 'CoOperation's.
instance
  (FreeOps ops1, FreeOps ops2) =>
  FreeOps (Union ops1 ops2)
   where
    mkFreeOps liftReturn = UnionOps ops1 ops2
     where
      ops1 = mkFreeOps (liftReturn . LeftCoOp)
      ops2 = mkFreeOps (liftReturn . RightCoOp)
