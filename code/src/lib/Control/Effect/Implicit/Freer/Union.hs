{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Freer.Union
  ( UnionCoOp (..)
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Freer.CoOp
import Control.Effect.Implicit.Freer.FreeOps

-- | The 'CoOperation' of @ops1 '∪' ops2@ under return type
-- @r@ is the sum of the underlying @'CoOperation' ops1 r@ and
-- @'CoOperation' ops2 r@.
data UnionCoOp coop1 coop2 r
  = LeftOp (coop1 r)
  | RightOp (coop2 r)

instance
  (EffCoOp ops1, EffCoOp ops2)
  => EffCoOp (Union ops1 ops2) where
    type CoOperation (Union ops1 ops2) =
      UnionCoOp (CoOperation ops1) (CoOperation ops2)

instance (Functor ops1, Functor ops2)
  => Functor (UnionCoOp ops1 ops2)
  where
    fmap f (LeftOp x) = LeftOp $ fmap f x
    fmap f (RightOp x) = RightOp $ fmap f x

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
      ops1 = mkFreeOps (liftReturn . LeftOp)
      ops2 = mkFreeOps (liftReturn . RightOp)
