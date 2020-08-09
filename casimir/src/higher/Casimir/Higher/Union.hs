{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# Language UndecidableInstances #-}

module Casimir.Higher.Union
where

import Data.Kind

import Casimir.Higher.Base
import Casimir.Higher.Free

data UnionCoOp coop1 coop2
  (f :: Type -> Type)
  r
  = LeftOp (coop1 f r)
  | RightOp (coop2 f r)

instance
  ( Functor f
  , Functor (coop1 f)
  , Functor (coop2 f)
  )
  => Functor (UnionCoOp coop1 coop2 f) where
    fmap f (LeftOp op) = LeftOp $ fmap f op
    fmap f (RightOp op) = RightOp $ fmap f op

instance
  ( CoOpFunctor coop1
  , CoOpFunctor coop2
  )
  => CoOpFunctor (UnionCoOp coop1 coop2) where
    liftCoOp f (LeftOp op) = LeftOp $ liftCoOp f op
    liftCoOp f (RightOp op) = RightOp $ liftCoOp f op

instance
  ( FreeOps ops1
  , FreeOps ops2
  )
  => FreeOps (Union ops1 ops2) where
    type CoOperation (Union ops1 ops2) =
      UnionCoOp (CoOperation ops1) (CoOperation ops2)

    mkFreeOps liftReturn = Union ops1 ops2
     where
      ops1 = mkFreeOps (liftReturn . LeftOp)
      ops2 = mkFreeOps (liftReturn . RightOp)
