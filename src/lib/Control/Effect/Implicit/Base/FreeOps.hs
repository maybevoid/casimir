
module Control.Effect.Implicit.Base.FreeOps
  ( FreeOps (..)
  )
where

import Data.Kind
import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.EffFunctor

-- | A 'FreeOps' @ops@ has associated types that can be used for effect
-- operations and interpretations. We typically use dummy datatypes with
-- empty declaration for @ops@ to signify that @ops@ is only used for
-- tagging effects and are not used at the value level.
class
  ( Functor (CoOperation ops)
  , EffFunctor (Operation ops)
  )
  => FreeOps (ops :: Type) where

    -- | The operation type for @ops@ is consumed by computations to
    -- perform effect operations. The type is indexed by an 'Effect' type
    -- @eff@ that indicates under which monad can operations be performed.
    -- @'Operation' ops@ must be an 'EffFunctor' to lift effect operations
    -- to work on any lifted effect. This is similar to how lifting works
    -- in MonadTrans, except here we are lifting the operations manually
    -- instead of through typeclasses.
    type family Operation ops
      = (o :: (Type -> Type) -> Type) | o -> ops

    -- | The co-operation type for @ops@ is produced by computations when
    -- they perform effect operations under a free monad, and is used by
    -- effect interpreters such as 'Control.Effect.Implicit.Free.CoOpHandler'
    -- to interpret effect operations from a free monad. @'CoOperation ops'@
    -- must be a functor so that we can use it as the payload type in free
    -- monads such as 'Control.Effect.Implicit.Free.ChurchMonad' or
    -- 'Control.Effect.Implicit.Free.FreeMonad'
    type family CoOperation ops
      = (o :: (Type -> Type)) | o -> ops

    -- | The free ops constructor is used for generating free operations that
    -- can work under any free monad transformer @t@ and any 'Effect' @eff@.
    -- It is given a payload lifter that can lift a @'CoOperation' ops@ into
    -- the free monad transformed effect @t eff@, which can be used to
    -- construct a free @'Operation' ops@ under the effect @t eff@. 'mkFreeOps'
    -- is used by 'Control.Effect.Implicit.Free.FreeEff' instances so that
    -- they can create a free operation for any free monad.
    mkFreeOps
      :: forall t eff
      . ( Effect eff
        , Effect (t eff)
        )
      => (forall a . CoOperation ops a -> t eff a)
      -> Operation ops (t eff)
