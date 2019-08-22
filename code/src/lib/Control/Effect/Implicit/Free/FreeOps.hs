
module Control.Effect.Implicit.Free.FreeOps
  ( FreeOps (..)
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free.EffCoOp

-- | A 'FreeOps' @ops@ has associated types that can be used for effect
-- operations and interpretations. We typically use dummy datatypes with
-- empty declaration for @ops@ to signify that @ops@ is only used for
-- tagging effects and are not used at the value level.
class
  ( EffCoOp ops
  , Functor (CoOperation ops)
  , EffFunctor ops
  )
  => FreeOps ops where

    -- | The free ops constructor is used for generating free operations that
    -- can work under any free monad transformer @t@ and any 'Effect' @eff@.
    -- It is given a payload lifter that can lift a @'CoOperation' ops@ into
    -- the free monad transformed effect @t eff@, which can be used to
    -- construct a free @'Operation' ops@ under the effect @t eff@. 'mkFreeOps'
    -- is used by 'Control.Effect.Implicit.Free.FreeEff' instances so that
    -- they can create a free operation for any free monad.
    mkFreeOps
      :: forall eff
      . (Effect eff)
      => (forall a . CoOperation ops a -> eff a)
      -> ops eff
