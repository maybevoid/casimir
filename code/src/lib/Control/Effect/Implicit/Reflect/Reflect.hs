{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Reflect.Reflect
where

import Data.Kind
import Data.Coerce
import Unsafe.Coerce
import Data.Reflection

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Cast

class
  ( Effect eff
  , ImplicitOps ops
  )
  => ReifiesOps ops eff
  where
    reflectOps :: Operation ops eff

instance
  ( Effect eff
  , ImplicitOps ops1
  , ImplicitOps ops2
  , EntailOps ops1 ops2
  , Given (Operation ops1 eff)
  )
  => ReifiesOps ops2 eff
  where
    reflectOps = castOps
      @eff @ops1 @ops2
      (entailOps @ops1 @ops2)
      given

class
  ( ImplicitOps ops
  , forall eff a
     . (Coercible (ReflectM ops eff a) (eff a))
  , forall eff
     . (Effect eff, ReifiesOps ops eff)
    => OpsClass ops (ReflectM ops eff)
  )
  => ReflectOps ops
  where
    type ReflectM ops (eff :: Type -> Type)
      :: Type -> Type

    type OpsClass ops (eff :: Type -> Type) =
      (c :: Constraint) | c -> ops eff

castClass
  :: forall eff ops
   . ( Effect eff
     , ImplicitOps ops
     , ReflectOps ops
     )
  => Cast (OpsClass ops (ReflectM ops eff))
  -> Cast (OpsClass ops eff)
castClass = unsafeCoerce