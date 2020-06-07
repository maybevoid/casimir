{-# LANGUAGE PolyKinds #-}

module Casimir.Free.FreeOps
  ( FreeOps (..)
  )
where

import Casimir.Base
import Casimir.Free.CoOp

-- | A 'FreeOps' @ops@ has associated types that can be used for mect
-- operations and interpretations. We typically use dummy datatypes with
-- empty declaration for @ops@ to signify that @ops@ is only used for
-- tagging mects and are not used at the value level.
class
  ( Effect ops
  , EffCoOp ops
  , Functor (CoOperation ops)
  )
  => FreeOps ops where

    -- | The free ops constructor is used for generating free operations that
    -- can work under any free monad transformer @t@ and any 'Monad' @m@.
    -- It is given a payload lifter that can lift a @'CoOperation' ops@ into
    -- the free monad transformed mect @t m@, which can be used to
    -- construct a free @'Operation' ops@ under the mect @t m@. 'mkFreeOps'
    -- is used by 'Casimir.Free.FreeEff' instances so that
    -- they can create a free operation for any free monad.
    mkFreeOps
      :: forall m
      . (Monad m)
      => (forall a . CoOperation ops a -> m a)
      -> Operation ops m
