{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.FreeEff
where

import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT)

import Control.Effect.Base.Effect

data OpsHandler handler a r eff = OpsHandler {
  handleReturn :: a -> eff r,
  handleOps :: CoOperation handler (eff r) -> eff r
}

class FreeEff (ops :: *) where
  type family Operation ops
    = (f :: (* -> *) -> *) | f -> ops

  type family CoOperation ops
    = (f :: (* -> *)) | f -> ops

  freeOps
    :: forall ops' eff .
    (Functor ops', Effect eff)
    => (CoOperation ops ~> ops')
    -> Operation ops (FreeT ops' eff)

type Model ops = CoOperation ops