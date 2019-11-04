
module Control.Effect.Implicit.Freer.FreeEff
  ( FreeEff (..)
  , FreerCoOpHandler (..)
  )
where

import Control.Effect.Implicit.Base

import Control.Effect.Implicit.Freer.CoOp
import Control.Effect.Implicit.Freer.FreeOps

data FreerCoOpHandler handler a r eff =
  FreerCoOpHandler {
    returnHandler :: a -> eff r,
    coOpHandler
      :: forall x
       . CoOperation handler x
      -> (x -> (eff r))
      -> eff r
  }

class
  (forall ops eff . (FreeOps ops, Effect eff) => Monad (free ops eff))
  => FreeEff free where
    freeOps :: forall ops eff
      . (FreeOps ops, Effect eff)
      => Operation ops (free ops eff)

    liftFree :: forall ops eff a
       . (FreeOps ops, Effect eff)
      => eff a
      -> free ops eff a

    handleFree
      :: forall ops eff a r
      . (Effect eff, FreeOps ops)
      => FreerCoOpHandler ops a r eff
      -> free ops eff a
      -> eff r
