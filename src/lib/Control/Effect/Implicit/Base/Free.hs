
module Control.Effect.Implicit.Base.Free
  ( FreeOps (..)
  )
where

import Data.Kind
import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.EffFunctor
import Control.Effect.Implicit.Base.Spec

-- | A 'FreeOps' @ops@ has associated types that can be used for effect
-- operations and interpretations. We typically use dummy datatypes with
-- empty declaration for @ops@ to signify that @ops@ is only used for
-- tagging effects and are not used at the value level.
class
  ( EffOps ops
  , EffCoOp ops
  , Functor (CoOperation ops)
  , EffFunctor (Operation ops)
  )
  => FreeOps (ops :: Type) where

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
