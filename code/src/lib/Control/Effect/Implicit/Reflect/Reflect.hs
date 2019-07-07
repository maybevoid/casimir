{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Reflect.Reflect
where

import Data.Kind
import Unsafe.Coerce
import Data.Constraint
import Data.Reflection

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Cast

newtype ReflectM ops eff a =
  ReflectM (eff a)
  deriving (Functor, Applicative, Monad)

class
  ( Effect eff
  , ImplicitOps ops
  )
  => ReifiesOps ops eff
  where
    reflectOps :: Operation ops eff

instance
  ( Effect eff
  , ImplicitOps ops
  , Given (Operation ops eff)
  )
  => ReifiesOps ops eff
  where
    reflectOps = given

class
  (ImplicitOps ops)
  => ReflectOps ops
  where
    type OpsClass ops (eff :: Type -> Type) =
      (c :: Constraint) | c -> ops eff

    opsDict
      :: forall eff
       . (Effect eff)
      => Operation ops eff
      -> Dict (OpsClass ops eff)

instance
  ( ReflectOps ops1, ReflectOps ops2 )
  => ReflectOps (ops1 ∪ ops2)
  where
    type OpsClass (ops1 ∪ ops2) eff =
      (OpsClass ops1 eff, OpsClass ops2 eff)

    opsDict (UnionOps ops1 ops2) =
      mergeDict (opsDict ops1) (opsDict ops2)

withReifiedOps
  :: forall ops eff r
   . (ImplicitOps ops, Effect eff)
  => Operation ops eff
  -> ((ReifiesOps ops eff) => r)
  -> r
withReifiedOps ops cont = give ops cont

castClass
  :: forall eff ops
   . ( Effect eff
     , ReflectOps ops
     )
  => Dict (OpsClass ops (ReflectM ops eff))
  -> Dict (OpsClass ops eff)
castClass = unsafeCoerce

reflectComputation
  :: forall ops eff a
   . (Effect eff, ReflectOps ops)
  => (OpsClass ops eff => eff a)
  -> (OpsConstraint ops eff => eff a)
reflectComputation comp =
  case (opsDict @ops @eff $ captureOps) of
    Dict -> comp