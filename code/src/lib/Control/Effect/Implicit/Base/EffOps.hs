module Control.Effect.Implicit.Base.EffOps
  ( EffOps (..)
  )
where

import Data.Kind

-- | An effect specification is consist of a dummy datatype @sig@, an operation
-- type and a co-operation type associated with the @sig@ type. The class
-- 'EffOps' represents the effect operation with an injective type family
-- 'Operation.
class EffOps sig where

  -- | The operation type for @sig@ is consumed by computations to
  -- perform effect operations. The type is indexed by an 'Effect' type
  -- @eff@ that indicates under which monad can the operations be performed.
  --
  -- @'Operation' sig@ must be an 'EffFunctor' to lift effect operations
  -- to work on any lifted monad. This is similar to how lifting works
  -- in MonadTrans, except here we are lifting the operations manually
  -- instead of through typeclasses.
  type family Operation sig
    = (ops :: (Type -> Type) -> Type) | ops -> sig
