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

data CoOpHandler handler a r m = CoOpHandler {
  returnHandler :: a -> m r,
  coOpHandler :: CoOperation handler (m r) -> m r
}

class
  ( forall ops m
    . (FreeOps ops, Monad m)
    => Monad (free ops m)
  )
  => FreeEff free
  where
    freeOps :: forall ops m
       . (FreeOps ops, Monad m)
      => Operations ops (free ops m)

    liftFree :: forall ops m a
       . (FreeOps ops, Monad m)
      => m a
      -> free ops m a

class
  (FreeEff free)
  => FreeHandler free
   where
    handleFree
      :: forall ops m a r
      . (Monad m, FreeOps ops)
      => CoOpHandler ops a r m
      -> free ops m a
      -> m r

newtype GenericCoOpHandler handler m
  = GenericCoOpHandler
    (forall a . CoOpHandler handler a a m)

data ContextualHandler w handler m = ContextualHandler {
  runContextualHandler
    :: forall a . CoOpHandler handler a (w m a) m,

  extractResult :: forall a . w m a -> m a
}

freeLiftEff
  :: forall free ops m
   . (FreeEff free, FreeOps ops, Monad m)
  => Lift m (free ops m)
freeLiftEff = Lift liftFree
