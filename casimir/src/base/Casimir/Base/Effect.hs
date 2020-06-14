{-# LANGUAGE UndecidableSuperClasses #-}

module Casimir.Base.Effect
  ( Effect
  , Effects
  , Union (..)
  , Cons (..)
  , Nil (..)
  , Singleton (..)
  , Multi
  , OpsConstraint
  , EffConstraint
  , Eff
  , withOps
  , captureOps
  , withOp
  , captureOp
  , type (∪)
  , pattern (:+)
  )
where

import Data.Kind

import QuasiParam.Label (HasLabel (..))
import Casimir.Param
  ( Union (..)
  , Cons (..)
  , Nil (..)
  , Singleton (..)
  , MultiParam (..)
  , Params
  )

class (MultiParam ops) => Effects ops
instance (MultiParam ops) => Effects ops

class (HasLabel ops) => Effect (ops :: (Type -> Type) -> Type)
instance (HasLabel ops) => Effect ops

pattern (:+) :: forall ops1 ops2 m
     . ops1 m
    -> ops2 m
    -> Cons ops1 ops2 m
pattern ops1 :+ ops2 = Cons ops1 ops2

type Multi ops = Params ops

infixr 7 ∪
type (∪) = Union

class (Effects ops, ParamConstraint ops m) => OpsConstraint ops m
instance (Effects ops, ParamConstraint ops m) => OpsConstraint ops m

class (Monad m, OpsConstraint (Multi ops) m) => EffConstraint ops m
instance (Monad m, OpsConstraint (Multi ops) m) => EffConstraint ops m

type Eff effs a = forall m . (EffConstraint effs m) => m a

captureOps
  :: forall ops m
   . ( OpsConstraint ops m )
  => ops m
captureOps = captureParam

withOps
  :: forall ops m r
   . ( Effects ops )
  => ops m
  -> ((OpsConstraint ops m) => r)
  -> r
withOps = withParam

captureOp
  :: forall ops m
   . ( EffConstraint '[ops] m )
  => ops m
captureOp = ops2
 where
  ops2 :+ _ = captureOps @(Multi '[ops])

withOp
  :: forall ops m r
   . ( Effect ops )
  => ops m
  -> ((OpsConstraint (Multi '[ops]) m) => r)
  -> r
withOp ops = withOps $ ops :+ Nil
