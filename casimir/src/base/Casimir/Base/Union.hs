{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Casimir.Base.Union
  ( Union
  , UnionOps
  , (∪)
  , type (∪)
  , leftOps
  , rightOps
  , pattern Union
  )
where

import qualified QuasiParam.Casimir as Param

import Casimir.Base.Effect
import Casimir.Base.EffFunctor

data Union eff1 eff2

type UnionOps = Param.Union

pattern Union
  :: forall ops1 ops2 m
   . ops1 m
  -> ops2 m
  -> UnionOps ops1 ops2 m
pattern Union ops1 ops2 = Param.Union ops1 ops2
{-# COMPLETE Union #-}

infixr 7 ∪
infixr 7 `Union`

type (∪) = Union

(∪) :: forall ops1 ops2 m
     . ops1 m
    -> ops2 m
    -> UnionOps ops1 ops2 m
(∪) = Union

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
    effmap f (Union x y)
      = Union (effmap f x) (effmap f y)

leftOps
  :: forall ops1 ops2 m
   . UnionOps ops1 ops2 m
  -> ops1 m
leftOps (Union ops _) = ops

rightOps
  :: forall ops1 ops2 m
   . UnionOps ops1 ops2 m
  -> ops2 m
rightOps (Union _ ops) = ops
