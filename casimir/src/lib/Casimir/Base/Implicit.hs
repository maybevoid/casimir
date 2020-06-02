{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Casimir.Base.Implicit
  ( ImplicitOps (..)
  , OpsConstraint
  , EffConstraint
  , Eff
  , EntailOps (..)
  , AllCoercible (..)
  , type (⊇)
  , withOps
  , captureOps
  )
where

import Data.Kind
import Data.Coerce

import Casimir.Base.NoOp
import Casimir.Base.Union
import Casimir.Base.Label
import Casimir.Base.EffOps

import qualified Data.QuasiParam.Multi as Multi

class
  ( forall m . Coercible (ops1 m) (ops2 m)
  , forall m . Coercible (ops2 m) (ops1 m)
  )
  => AllCoercible
      (ops1 :: (Type -> Type) -> Type)
      (ops2 :: (Type -> Type) -> Type)
  where
    coerceTo :: forall m . ops1 m -> ops2 m

    coerceFrom :: forall m . ops2 m -> ops1 m

    entailCoercible
      :: forall m r
       . ( (Coercible (ops1 m) (ops2 m)
           , Coercible (ops2 m) (ops1 m)
           ) => r)
      -> r

instance
  ( forall m . Coercible (ops1 m) (ops2 m)
  , forall m . Coercible (ops2 m) (ops1 m)
  )
  => AllCoercible
      (ops1 :: (Type -> Type) -> Type)
      (ops2 :: (Type -> Type) -> Type)
  where
    coerceTo :: forall m . ops1 m -> ops2 m
    coerceTo = coerce

    coerceFrom :: forall m . ops2 m -> ops1 m
    coerceFrom = coerce

    entailCoercible cont = cont

class
  ( Multi.MultiParam (Type -> Type) (AsMultiParam ops)
  , AllCoercible ops (AsMultiParam ops)
  )
  => ParamOps ops where
    type family AsMultiParam ops
      :: (Type -> Type) -> Type

type OpsParam ops m =
  Multi.ParamConstraint
    (Type -> Type)
    (AsMultiParam ops)
    m

instance ParamOps NoOp where
  type AsMultiParam NoOp = Multi.Empty (Type -> Type)

instance ParamOps (LabeledOps k label ops) where
  type AsMultiParam (LabeledOps k label ops) =
    Multi.Elem k (Type -> Type) label ops

instance
  ( ParamOps ops1
  , ParamOps ops2
  , AsMultiParam ops1 ~ ops1'
  , AsMultiParam ops2 ~ ops2'
  , Multi.Cons (Type -> Type) ops1' ops2' ~ ops3
  , AllCoercible (UnionOps ops1 ops2) (ops3)
  )
  => ParamOps (UnionOps ops1 ops2) where
    type AsMultiParam (UnionOps ops1 ops2) =
      Multi.Cons
        (Type -> Type)
        (AsMultiParam ops1)
        (AsMultiParam ops2)

class
  ( EffOps eff
  , ParamOps (CanonOps eff)
  , AllCoercible (Operation eff) (CanonOps eff)
  )
  => ImplicitOps eff where
    type family CanonOps eff
      :: (Type -> Type) -> Type

instance
  ( EffOps eff )
  => ImplicitOps (LabeledEff k label eff) where
    type CanonOps (LabeledEff k label eff)
      = LabeledOps k label (Operation eff)

instance ImplicitOps NoEff where
  type CanonOps NoEff = NoOp

instance
  ( ImplicitOps eff1
  , ImplicitOps eff2
  , Operation eff1 ~ ops11
  , CanonOps eff1 ~ ops12
  , Operation eff2 ~ ops21
  , CanonOps eff2 ~ ops22
  , AsMultiParam ops12 ~ ops13
  , AsMultiParam ops22 ~ ops23
  , Multi.Cons (Type -> Type) ops13 ops23 ~ ops3
  , AllCoercible
      (UnionOps ops12 ops22)
      (ops3)
  , AllCoercible
      (UnionOps ops11 ops21)
      (UnionOps ops12 ops22)
  )
  => ImplicitOps (Union eff1 eff2) where
    type CanonOps (Union eff1 eff2) =
      UnionOps
        (CanonOps eff1)
        (CanonOps eff2)

class
  ( ImplicitOps eff1
  , ImplicitOps eff2
  )
  => EntailOps eff1 eff2 where
    castOps
      :: forall m
       . Operation eff1 m
      -> Operation eff2 m

instance
  ( ImplicitOps eff1
  , ImplicitOps eff2
  , CanonOps eff1 ~ ops11
  , CanonOps eff2 ~ ops21
  , AsMultiParam ops11 ~ ops12
  , AsMultiParam ops21 ~ ops22
  , Multi.CastParam (Type -> Type) ops12 ops22
  )
  => EntailOps eff1 eff2 where
    castOps
      :: forall m
       . Operation eff1 m
      -> Operation eff2 m
    castOps ops1 = coerceFrom ops5
     where
      ops2 :: ops11 m
      ops2 = coerceTo ops1

      ops3 :: ops12 m
      ops3 = coerceTo ops2

      ops4 :: ops22 m
      ops4 = Multi.castValue ops3

      ops5 :: ops21 m
      ops5 = coerceFrom ops4

infixl 6 ⊇
type ops1 ⊇ ops2 = EntailOps ops1 ops2

type OpsConstraint ops m =
  ( ImplicitOps ops
  , OpsParam (CanonOps ops) m
  )

type EffConstraint ops m = (Monad m, OpsConstraint ops m)

type Eff ops a = forall m . (EffConstraint ops m) => m a

withOps'
  :: forall ops m r
   . ( ParamOps ops )
  => ops m
  -> (OpsParam ops m => r)
  -> r
withOps' ops cont = Multi.withParam ops' cont
 where
  ops' :: AsMultiParam ops m
  ops' = coerceTo ops

captureOps'
  :: forall ops m
   . ( ParamOps ops
     , OpsParam ops m
     )
  => ops m
captureOps' = coerceFrom ops'
 where
  ops' :: AsMultiParam ops m
  ops' = Multi.captureParam @(Type -> Type) @(AsMultiParam ops)

withOps
  :: forall m ops r
   . ( ImplicitOps ops
     )
  => Operation ops m
  -> (OpsConstraint ops m => r)
  -> r
withOps ops1 = withOps' ops2
 where
  ops2 :: CanonOps ops m
  ops2 = coerceTo ops1

captureOps
  :: forall ops m
   . ( OpsConstraint ops m )
  => Operation ops m
captureOps = coerceFrom ops
 where
  ops :: CanonOps ops m
  ops = captureOps'
