
module Control.Effect.Implicit.Freer.FreeOps
  ( FreerOps (..)
  )
where

import Data.Kind
import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Freer.EffCoOp

class
  ( EffOps ops
  , FreerEffCoOp ops
  , EffFunctor (Operation ops)
  )
  => FreerOps (ops :: Type) where
    mkFreerOps
      :: forall t eff
        . ( Effect eff
          , Effect (t eff)
          )
      => (forall a . FreerCoOp ops a -> t eff a)
      -> Operation ops (t eff)
