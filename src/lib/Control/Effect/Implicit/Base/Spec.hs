
module Control.Effect.Implicit.Base.Spec
  ( EffOps (..)
  , EffCoOp (..)
  )
where

import Data.Kind

class EffOps sig where

  -- | The operation type for @ops@ is consumed by computations to
  -- perform effect operations. The type is indexed by an 'Effect' type
  -- @eff@ that indicates under which monad can the operations be performed.
  -- @'Operation' ops@ must be an 'EffFunctor' to lift effect operations
  -- to work on any lifted monad. This is similar to how lifting works
  -- in MonadTrans, except here we are lifting the operations manually
  -- instead of through typeclasses.
  type family Operation sig
    = (ops :: (Type -> Type) -> Type) | ops -> sig

class EffCoOp sig where
  -- | The co-operation type for @ops@ is produced by computations when
  -- they perform effect operations under a free monad, and is used by
  -- effect interpreters such as 'Control.Effect.Implicit.Free.CoOpHandler'
  -- to interpret effect operations from a free monad. @'CoOperation ops'@
  -- must be a functor so that we can use it as the payload type in free
  -- monads such as 'Control.Effect.Implicit.Free.ChurchMonad' or
  -- 'Control.Effect.Implicit.Free.FreeMonad'
  type family CoOperation sig
    = (coop :: (Type -> Type)) | coop -> sig

