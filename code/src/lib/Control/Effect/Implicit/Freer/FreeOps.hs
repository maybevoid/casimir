
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
      :: forall t eff
        . ( Effect eff
          , Effect (t eff)
          )
      => (forall a . CoOperation ops a -> t eff a)
      -> ops (t eff)
