
module Casimir.Free.CoOp
  ( EffCoOp (..)
  )
where

import Data.Kind

-- | An effect specification is consist of a dummy datatype @sig@, an operation
-- type and a co-operation type associated with the @sig@ type. The class
-- 'EffCoOp' represents the effect co-operation with an injective type family
-- 'CoOperation.
class EffCoOp ops where
  -- | The co-operation type for @sig@ is produced by computations when
  -- they perform effect operations under a free monad, and is used by
  -- effect interpreters such as 'Casimir.Free.CoOpHandler'
  -- to interpret effect operations from a free monad.

  -- @'CoOperation sig'@ must be a functor so that we can use it as the
  -- payload type in free monads such as
  -- 'Casimir.Free.ChurchMonad' or
  -- 'Casimir.Free.FreeMonad'
  type family CoOperation ops
    = (coop :: Type -> Type) | coop -> ops
