
module Control.Effect.Implicit.Freer.FreeOps
  ( FreeOps (..)
  )
where

import Data.Kind
import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Freer.EffCoOp

class
  ( EffOps ops
  , EffCoOp ops
  , EffFunctor (Operation ops)
  )
  => FreeOps (ops :: Type) where
    mkFreeOps
      :: forall t eff
        . ( Effect eff
          , Effect (t eff)
          )
      => (forall a . CoOperation ops a -> t eff a)
      -> Operation ops (t eff)
