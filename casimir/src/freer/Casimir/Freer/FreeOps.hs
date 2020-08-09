{-# language PolyKinds #-}

module Casimir.Freer.FreeOps
  ( FreeOps (..)
  , NoCoOp
  , UnionCoOp (..)
  )
where

import Data.Kind
import Casimir.Base

data NoCoOp r

data UnionCoOp coop1 coop2 r
  = LeftOp (coop1 r)
  | RightOp (coop2 r)

class FreeOps (ops :: (Type -> Type) -> Type) where
  type family CoOperation ops :: (Type -> Type)

  mkFreeOps
    :: forall m
     . (Monad m)
    => (forall a . CoOperation ops a -> m a)
    -> ops m

instance
  ( FreeOps ops1, FreeOps ops2 )
  => FreeOps (Union ops1 ops2) where
    type CoOperation (Union ops1 ops2) =
      UnionCoOp (CoOperation ops1) (CoOperation ops2)

    mkFreeOps liftReturn = Union ops1 ops2
     where
      ops1 = mkFreeOps (liftReturn . LeftOp)
      ops2 = mkFreeOps (liftReturn . RightOp)

instance
  ( FreeOps ops1, FreeOps ops2 )
  => FreeOps (Cons ops1 ops2) where
    type CoOperation (Cons ops1 ops2) =
      UnionCoOp (CoOperation ops1) (CoOperation ops2)

    mkFreeOps liftReturn = Cons ops1 ops2
     where
      ops1 = mkFreeOps (liftReturn . LeftOp)
      ops2 = mkFreeOps (liftReturn . RightOp)

instance FreeOps Nil where
  type CoOperation Nil = NoCoOp

  mkFreeOps _ = Nil

instance Functor NoCoOp where
  fmap _ coop = case coop of {}

instance (Functor ops1, Functor ops2)
  => Functor (UnionCoOp ops1 ops2)
  where
    fmap f (LeftOp x) = LeftOp $ fmap f x
    fmap f (RightOp x) = RightOp $ fmap f x
