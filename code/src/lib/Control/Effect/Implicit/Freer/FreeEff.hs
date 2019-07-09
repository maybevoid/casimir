
module Control.Effect.Implicit.Freer.FreeEff
  ( FreeEff (..)
  , FreerCoOpHandler (..)
  , CoOpCont (..)
  )
where

import Control.Effect.Implicit.Base

import Control.Effect.Implicit.Freer.EffCoOp
import Control.Effect.Implicit.Freer.FreeOps

data CoOpCont ops a where
  CoOpCont
    :: forall ops a x
      . CoOperation ops x -> (x -> a) -> CoOpCont ops a

data FreerCoOpHandler handler a r eff =
  FreerCoOpHandler {
    returnHandler :: a -> eff r,
    coOpHandler :: CoOpCont handler (eff r) -> eff r
  }

class
  (forall ops eff . (FreeOps ops, Effect eff) => Monad (free ops eff))
  => FreeEff free where
    freeOps :: forall ops eff
      . (FreeOps ops, Effect eff)
      => Operation ops (free ops eff)

    liftFree :: forall ops eff a
      . (FreeOps ops, Effect eff)
      => eff a -> free ops eff a

    handleFree
      :: forall ops eff a r
      . (Effect eff, FreeOps ops)
      => FreerCoOpHandler ops a r eff
      -> free ops eff a
      -> eff r
