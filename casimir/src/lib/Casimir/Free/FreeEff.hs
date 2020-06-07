{-# LANGUAGE PolyKinds #-}

module Casimir.Free.FreeEff
  ( FreeEff (..)
  , FreeHandler (..)
  , CoOpHandler (..)
  , GenericCoOpHandler (..)
  , ContextualHandler (..)
  , freeLiftEff
  )
where

import Casimir.Base
import Casimir.Free.CoOp
import Casimir.Free.FreeOps

data CoOpHandler handler a r eff = CoOpHandler {
  returnHandler :: a -> eff r,
  coOpHandler :: CoOperation handler (eff r) -> eff r
}

class
  ( forall ops eff
    . (FreeOps ops, Monad eff)
    => Monad (free ops eff)
  )
  => FreeEff free
  where
    freeOps :: forall ops eff
       . (FreeOps ops, Monad eff)
      => Operation ops (free ops eff)

    liftFree :: forall ops eff a
       . (FreeOps ops, Monad eff)
      => eff a
      -> free ops eff a

class
  (FreeEff free)
  => FreeHandler free
   where
    handleFree
      :: forall ops eff a r
      . (Monad eff, FreeOps ops)
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
   . (FreeEff free, FreeOps ops, Monad eff)
  => Lift eff (free ops eff)
freeLiftEff = Lift liftFree
