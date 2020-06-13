{-# language PolyKinds #-}

module Casimir.Base.Effect
  ( Effect (..)
  , Effects (..)
  , NoEff
  , Union
  , NoOp
  , List
  , Singleton
  , ConsOps
  , UnionOps
  , SingleOp
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

import QuasiParam.Label (HasLabel (..))
import qualified Casimir.Param as Param


data List (effs :: [Type])
data Singleton (eff :: Type)
data Union (eff1 :: Type) (eff2 :: Type)

type NoEff = List '[]
type NoOp = Param.Nil
type SingleOp = Param.Singleton
type UnionOps = Param.Union
type ConsOps = Param.Cons

class
  ( Param.MultiParam (Operations effs) )
  => Effects (effs :: Type) where
    type family Operations effs
      = (ops :: (Type -> Type) -> Type) | ops -> effs

class
  ( HasLabel (Operation eff) )
  => Effect (eff :: Type) where
  type family Operation eff
    = (ops :: (Type -> Type) -> Type) | ops -> eff

instance
  ( Effects effs1
  , Effects effs2
  )
  => Effects (Union effs1 effs2) where
    type Operations (Union effs1 effs2) =
      Param.Union (Operations effs1) (Operations effs2)

instance
  ( Effect eff
  , Effects (List effs)
  )
  => Effects (List (eff ': effs)) where
    type Operations (List (eff ': effs)) =
      Param.Cons (Operation eff) (Operations (List effs))

instance
  ( Effect eff )
  => Effects (Singleton eff) where
    type Operations (Singleton eff) =
      Param.Singleton (Operation eff)

instance Effects (List '[]) where
  type Operations (List '[]) = Param.Nil

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
