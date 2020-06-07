{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Casimir.Base.Union
  ( Union
  , UnionOps
  , (∪)
  , type (∪)
  , leftOps
  , rightOps
  , pattern UnionOps
  )
where

import qualified QuasiParam.Casimir as Param

import Casimir.Base.Effect
import Casimir.Base.EffFunctor

data Union eff1 eff2

type UnionOps = Param.Union

pattern UnionOps
  :: forall ops1 ops2 m
   . ops1 m
  -> ops2 m
  -> UnionOps ops1 ops2 m
pattern UnionOps ops1 ops2 = Param.Union ops1 ops2
{-# COMPLETE UnionOps #-}

infixr 7 ∪
infixr 7 `Union`

type (∪) = Union

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

leftOps
  :: forall ops1 ops2 m
   . UnionOps ops1 ops2 m
  -> ops1 m
leftOps (UnionOps ops _) = ops

rightOps
  :: forall ops1 ops2 m
   . UnionOps ops1 ops2 m
  -> ops2 m
rightOps (UnionOps _ ops) = ops
