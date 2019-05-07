
module Control.Effect.Implicit.Freer.FreerEff
  ( FreerEff (..)
  , FreerCoOpHandler (..)
  , CoOpCont (..)
  )
where

import Control.Effect.Implicit.Base

data CoOpCont ops a where
  CoOpCont
    :: forall ops a x
      . FreerCoOp ops x -> (x -> a) -> CoOpCont ops a

data FreerCoOpHandler handler a r eff =
  FreerCoOpHandler {
    handleFreerReturn :: a -> eff r,
    handleFreerCoOp :: CoOpCont handler (eff r) -> eff r
  }

class
  (forall ops eff . (FreerOps ops, Effect eff) => Monad (free ops eff))
  => FreerEff free where
    freerOps :: forall ops eff
      . (FreerOps ops, Effect eff)
      => Operation ops (free ops eff)

    liftFreer :: forall ops eff a
      . (FreerOps ops, Effect eff)
      => eff a -> free ops eff a

    handleFreer
      :: forall ops eff a r
      . (Effect eff, FreerOps ops)
      => FreerCoOpHandler ops a r eff
      -> free ops eff a
      -> eff r
