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
  , EffOps ops
  )
  => ReifiesOps ops eff
  where
    reflectOps :: Operation ops eff

instance
  ( Effect eff
  , EffOps ops
  , Given (Operation ops eff)
  )
  => ReifiesOps ops eff
  where
    reflectOps = given

class
  (EffOps ops)
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
   . (EffOps ops, Effect eff)
  => Operation ops eff
  -> ((ReifiesOps ops eff) => r)
  -> r
withReifiedOps ops cont = give ops cont

castOpsClass
  :: forall eff ops
   . ( Effect eff
     , ReflectOps ops
     )
  => Dict (OpsClass ops (ReflectM ops eff))
  -> Dict (OpsClass ops eff)
castOpsClass = unsafeCoerce

reflectComputation
  :: forall ops eff r
   . ( Effect eff
     , ReflectOps ops
     , ImplicitOps ops
     )
  => (OpsClass ops eff => r)
  -> (OpsConstraint ops eff => r)
reflectComputation comp =
  case (opsDict @ops @eff $ captureOps) of
    Dict -> comp


reflectGeneric
  :: forall ops1 ops2 a
  . ( ReflectOps ops2
    , ImplicitOps ops1
    , ImplicitOps ops2
    )
  => (forall eff .
        ( Effect eff
        , OpsConstraint ops1 eff
        , OpsClass ops2 eff
        )
      => eff a)
  -> Eff (ops1 ∪ ops2) a
reflectGeneric _ = undefined
