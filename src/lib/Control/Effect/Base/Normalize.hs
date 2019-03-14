{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Base.Normalize
  ( Normalizable (..)
  , UnionT
  , UnionConstraint
  , normalizeOps
  )
where

import Data.Kind (Constraint)

import Control.Effect.Base.Union
import Control.Effect.Base.NoEff
import Control.Effect.Base.EffOps
import Control.Effect.Base.Effect
import Control.Effect.Base.FreeOps

type family UnionT ops1 ops2 where
  UnionT NoEff ops = ops

  UnionT (Union ops1 ops2) ops3
    = UnionT ops1 (UnionT ops2 ops3)

  UnionT ops1 ops2 = Union ops1 ops2

type family UnionConstraint ops1 ops2 :: Constraint where
  UnionConstraint NoEff ops2 = ()

  UnionConstraint (Union ops1 ops2) ops3 =
    ( Normalizable (UnionT ops2 ops3)
    , UnionConstraint ops1 (UnionT ops2 ops3)
    , UnionConstraint ops2 ops3
    )

  UnionConstraint ops1 ops2 = (UnionT ops1 ops2) ~ (Union ops1 ops2)

class (EffOps ops1) => Normalizable ops1 where
  unionOps
    :: forall ops2 eff .
    (Effect eff, Normalizable ops2, UnionConstraint ops1 ops2)
    => Operation ops1 eff
    -> Operation ops2 eff
    -> Operation (UnionT ops1 ops2) eff

instance Normalizable NoEff where
  unionOps NoOp ops = ops

instance
  ( Normalizable ops1
  , Normalizable ops2
  , EffOps ops1
  , EffOps ops2
  )
  => Normalizable (Union ops1 ops2)
  where
    unionOps
      :: forall ops3 eff .
      ( Effect eff
      , Normalizable ops3
      , Normalizable (UnionT ops2 ops3)
      , UnionConstraint ops1 (UnionT ops2 ops3)
      , UnionConstraint ops2 ops3
      )
      => Operation (Union ops1 ops2) eff
      -> Operation ops3 eff
      -> Operation (UnionT ops1 (UnionT ops2 ops3)) eff
    unionOps (UnionOps ops1 ops2) ops3 =
      unionOps @ops1 @(UnionT ops2 ops3) ops1 $
        unionOps @ops2 @ops3 ops2 ops3

normalizeOps
  :: forall ops eff .
  ( Normalizable ops
  , Effect eff
  , UnionConstraint ops NoEff
  )
  => Operation ops eff
  -> Operation (UnionT ops NoEff) eff
normalizeOps ops = unionOps ops NoOp
