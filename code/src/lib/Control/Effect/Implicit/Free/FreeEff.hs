
module Control.Effect.Implicit.Free.FreeEff
  ( FreeEff (..)
  , CoOpHandler (..)
  , GenericCoOpHandler (..)
  , ContextualHandler (..)
  , freeLiftEff
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free.EffCoOp
import Control.Effect.Implicit.Free.FreeOps

data CoOpHandler handler a r eff = CoOpHandler {
  handleFreeReturn :: a -> eff r,
  handleFreeCoOp :: CoOperation handler (eff r) -> eff r
}

class
  (forall ops eff . (FreeOps ops, Effect eff) => Monad (free ops eff))
  => FreeEff free
  where
    freeOps :: forall ops eff .
      (FreeOps ops, Effect eff)
      => Operation ops (free ops eff)

    liftFree :: forall ops eff a .
      (FreeOps ops, Effect eff)
      => eff a -> free ops eff a

    handleFree
      :: forall ops eff a r
      . (Effect eff, FreeOps ops)
      => CoOpHandler ops a r eff
      -> free ops eff a
      -> eff r

newtype GenericCoOpHandler handler eff
  = GenericCoOpHandler
    (forall a . CoOpHandler handler a a eff)

data ContextualHandler w handler eff = ContextualHandler {
  runContextualHandler
    :: forall a . CoOpHandler handler a (w eff a) eff,

  extractResult :: forall a . w eff a -> eff a
}

freeLiftEff
  :: forall free ops eff
   . (FreeEff free, FreeOps ops, Effect eff)
  => LiftEff eff (free ops eff)
freeLiftEff = mkLiftEff liftFree
