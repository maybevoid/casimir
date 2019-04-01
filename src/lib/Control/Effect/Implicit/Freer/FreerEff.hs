
module Control.Effect.Implicit.Freer.FreerEff
  ( FreerEff (..)
  , FreerCoOpHandler (..)
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free

data FreerCoOpHandler handler a r eff = FreerCoOpHandler {
  handleFreerReturn :: a -> eff r,
  handleFreerCoOp
    :: forall x
     . CoOperation handler x
     -> (x -> eff r)
     -> eff r
}

class (FreeEff free)
  => FreerEff free
  where
   handleFreer
      :: forall ops eff a r
      . (Effect eff, FreeOps ops)
      => FreerCoOpHandler ops a r eff
      -> free ops eff a
      -> eff r
