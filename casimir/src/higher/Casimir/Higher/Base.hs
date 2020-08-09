{-# language PolyKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Casimir.Higher.Base
  ( Effect
  , Effects
  , Union (..)
  , Cons (..)
  , Nil (..)
  , Singleton (..)
  , Multi
  , Operation
  , OpsConstraint
  , EffConstraint
  , HigherOps (..)
  , LowerOps (..)
  , Ops (..)
  -- , Eff
  , withOps
  , captureOps
  , withOp
  , captureOp
  , unOps
  , unHigherOps
  , type (∪)
  , pattern (:+)
  )
where

import Data.Kind

import Casimir.Higher.Param
  ( MultiParam (..)
  , Params
  , Nil (..)
  , Cons (..)
  , Union (..)
  , Singleton (..)
  )

import Casimir.Higher.Sig
import QuasiParam.Label (HasLabel (..))

type Operation ops m1 m2 = ops ('MonadPair m1 m2)

newtype LowerOps ops
  (m :: Type -> Type)
  = LowerOps
    { unLowerOps :: Operation ops m m }

data HigherOps
  (ops :: (Type -> Type) -> Type)
  (m :: MonadPair)
  where
    HigherOps
      :: forall ops m1 m2
      . ops m2
      -> HigherOps ops ('MonadPair m1 m2)

data Ops
  (ops :: (Type -> Type) -> (Type -> Type) -> Type)
  (m :: MonadPair)
   where
    Ops
      :: forall ops m1 m2
       . ops m1 m2
      -> Ops ops ('MonadPair m1 m2)

unOps
  :: forall ops m1 m2
   . Ops ops ('MonadPair m1 m2)
  -> ops m1 m2
unOps (Ops ops) = ops

unHigherOps
  :: forall ops m1 m2
   . Operation (HigherOps ops) m1 m2
  -> ops m2
unHigherOps (HigherOps ops) = ops

instance (HasLabel ops) => HasLabel (Ops ops) where
  type GetLabel (Ops ops) = GetLabel ops

instance (HasLabel ops) => HasLabel (LowerOps ops) where
  type GetLabel (LowerOps ops) = GetLabel ops

instance (HasLabel ops) => HasLabel (HigherOps ops) where
  type GetLabel (HigherOps ops) = GetLabel ops

class (MultiParam ops) => Effects ops
instance (MultiParam ops) => Effects ops

class (HasLabel ops)
  => Effect (ops :: MonadPair -> Type)
instance (HasLabel ops) => Effect ops

infixr 7 ∪
type (∪) = Union

pattern (:+)
  :: forall ops1 ops2 m1 m2
   . Operation ops1 m1 m2
  -> Operation ops2 m1 m2
  -> Operation (Cons ops1 ops2) m1 m2
pattern ops1 :+ ops2 = Cons ops1 ops2

class
  ( Effects ops
  , ParamConstraint ops ('MonadPair m1 m2)
  )
  => OpsConstraint ops m1 m2

instance
  ( Effects ops
  , ParamConstraint ops ('MonadPair m1 m2)
  )
  => OpsConstraint ops m1 m2

type Multi ops = Params ops


-- type family Multi
--   (xs :: [(Type -> Type) -> (Type -> Type) -> Type])
--   :: MonadPair -> Type
--   where
--     Multi '[] = Nil
--     Multi (x ': xs) = Param.Cons (Ops x) (Multi xs)

class
  ( Monad m1
  , Monad m2
  , ParamConstraint (Multi ops) ('MonadPair m1 m2)
  )
  => EffConstraint ops m1 m2

instance
  ( Monad m1
  , Monad m2
  , Effects (Multi ops)
  , ParamConstraint (Multi ops) ('MonadPair m1 m2)
  )
  => EffConstraint ops m1 m2

captureOps
  :: forall ops m1 m2
   . (OpsConstraint ops m1 m2)
  => Operation ops m1 m2
captureOps = captureParam

withOps
  :: forall ops m1 m2 r
   . ( Effects ops )
  => Operation ops m1 m2
  -> ((OpsConstraint ops m1 m2) => r)
  -> r
withOps = withParam

captureOp
  :: forall ops m1 m2
   . ( Effect ops
     , EffConstraint '[ops] m1 m2 )
  => Operation ops m1 m2
captureOp = ops
 where
  ops :+ _ = captureOps @(Multi '[ops])

withOp
  :: forall ops m1 m2 r
   . ( Effect ops )
  => Operation ops m1 m2
  -> ((OpsConstraint (Multi '[ops]) m1 m2) => r)
  -> r
withOp ops = withOps $ ops :+ Nil

-- data Singleton
--   (ops :: (Type -> Type) -> (Type -> Type) -> Type)
--   (m1 :: Type -> Type)
--   (m2 :: Type -> Type)
--   = Singleton { unSingleton :: ops m1 m2 }

-- data Union
--   (ops1 :: (Type -> Type) -> (Type -> Type) -> Type)
--   (ops2 :: (Type -> Type) -> (Type -> Type) -> Type)
--   (m1 :: Type -> Type)
--   (m2 :: Type -> Type)
--   = Union (ops1 m1 m2) (ops2 m1 m2)

-- data Cons
--   (ops1 :: (Type -> Type) -> (Type -> Type) -> Type)
--   (ops2 :: (Type -> Type) -> (Type -> Type) -> Type)
--   (m1 :: Type -> Type)
--   (m2 :: Type -> Type)
--   = Cons (ops1 m1 m2) (ops2 m1 m2)

-- class WrapOps
--   (ops :: (Type -> Type) -> (Type -> Type) -> Type)
--   where
--     type family WrappedOps ops
--       :: MonadPair -> Type

--     wrapOps ::

-- instance WrapOps (Singleton ops) where
--   type WrappedOps (Singleton ops) = Param.Singleton (Ops ops)

-- instance
--   ( WrapOps ops1
--   , WrapOps ops2
--   )
--   => WrapOps (Union ops1 ops2) where
--     type WrappedOps (Union ops1 ops2) =
--       Param.Union (WrappedOps ops1) (WrappedOps ops2)

-- import Casimir.Base
--   ( NoEff
--   , NoOp
--   , EffFunctor (..)
--   , HasLabel (..)
--   )

-- class Effects (eff :: k) where
--   type family Operations eff
--     = (ops :: (Type -> Type) -> (Type -> Type) -> Type)
--     | ops -> eff

-- class Effect (eff :: Type) where
--   type family Operation eff
--     = (ops :: (Type -> Type) -> (Type -> Type) -> Type)
--     | ops -> eff

-- instance
--   (EffFunctor lift ops)
--   => EffFunctor lift (HigherOps ops m) where
--     effmap lift (HigherOps ops) = HigherOps $
--       effmap lift ops

-- instance (HasLabel ops)
--   => HasLabel (LowerOps ops) where
--     type GetLabel (LowerOps ops) = GetLabel ops

-- instance Effects NoEff where
--   type Operations NoEff = HigherOps NoOp
