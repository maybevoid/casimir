
module Control.Effect.Implicit.Freer.FreeOps
  ( FreeOps (..)
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Freer.EffCoOp

class
  ( EffCoOp ops
  , EffFunctor ops
  )
  => FreeOps ops
   where
    mkFreeOps
      :: forall eff
        . (Effect eff)
      => (forall a . CoOperation ops a -> eff a)
      -> ops eff
