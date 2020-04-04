{-# LANGUAGE PolyKinds #-}

module Control.Effect.Implicit.Free.FreeEff
  ( FreeEff (..)
  , FreeHandler (..)
  , CoOpHandler (..)
  , GenericCoOpHandler (..)
  , ContextualHandler (..)
  , freeLiftEff
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free.CoOp
import Control.Effect.Implicit.Free.FreeOps

data CoOpHandler handler a r eff = CoOpHandler {
  returnHandler :: a -> eff r,
  coOpHandler :: CoOperation handler (eff r) -> eff r
}

class
  ( forall ops eff
    . (FreeOps ops, Effect eff)
    => Monad (free ops eff)
  )
  => FreeEff free
  where
    freeOps :: forall ops eff
       . (FreeOps ops, Effect eff)
      => Operation ops (free ops eff)

    liftFree :: forall ops eff a
       . (FreeOps ops, Effect eff)
      => eff a
      -> free ops eff a

class
  (FreeEff free)
  => FreeHandler free
   where
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
  => Lift eff (free ops eff)
freeLiftEff = Lift liftFree
