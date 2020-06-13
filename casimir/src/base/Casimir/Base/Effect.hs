{-# language PolyKinds #-}

module Casimir.Base.Effect
  ( Effect (..)
  , Effects (..)
  , NoEff
  , Union
  , Cons
  , NoOp
  , ConsOps
  , UnionOps
  , type (∪)
  , (∪)
  , pattern (:∪)
  , pattern (:+)
  , pattern NoOp
  , pattern Cons
  , pattern Union
  )
where

import Data.Kind

import qualified QuasiParam.Casimir as Param

data Cons (eff1 :: Type) (eff2 :: Type)
data Union (eff1 :: Type) (eff2 :: Type)

type NoEff = ('[] :: [Type])
type NoOp = Param.Nil
type UnionOps = Param.Union
type ConsOps = Param.Cons

class Effects (effs :: k) where
    type family Operations effs
      = (ops :: (Type -> Type) -> Type) | ops -> effs

class Effect (eff :: Type) where
  type family Operation eff
    = (ops :: (Type -> Type) -> Type) | ops -> eff

instance Effects ('[] :: [Type]) where
  type Operations '[] = Param.Nil

instance
  ( Effects effs1
  , Effects effs2
  )
  => Effects (Union effs1 effs2) where
    type Operations (Union effs1 effs2) =
      Param.Union (Operations effs1) (Operations effs2)

instance
  ( Effect eff
  , Effects effs
  )
  => Effects (eff ': effs) where
    type Operations (eff ': effs) =
      Param.Cons (Operation eff) (Operations effs)

pattern NoOp :: forall m . NoOp m
pattern NoOp = Param.Nil
{-# COMPLETE NoOp #-}

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

(∪)
  :: forall ops1 ops2 m
   . ops1 m
  -> ops2 m
  -> UnionOps ops1 ops2 m
(∪) = Union

pattern (:∪)
  :: forall ops1 ops2 m
     . ops1 m
    -> ops2 m
    -> UnionOps ops1 ops2 m
pattern ops1 :∪ ops2 = Union ops1 ops2

pattern Cons
  :: forall ops1 ops2 m
   . ops1 m
  -> ops2 m
  -> ConsOps ops1 ops2 m
pattern Cons ops1 ops2 = Param.Cons ops1 ops2

pattern (:+) :: forall ops1 ops2 m
     . ops1 m
    -> ops2 m
    -> ConsOps ops1 ops2 m
pattern ops1 :+ ops2 = Cons ops1 ops2
