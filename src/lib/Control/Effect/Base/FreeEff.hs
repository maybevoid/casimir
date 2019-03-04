{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.FreeEff
where

import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT)

import Control.Effect.Base.Effect

class FreeEff ops where
  type family FreeModel ops = (m :: (* -> *)) | m -> ops

  freeModel
    :: forall ops' eff .
    (Functor ops', Effect eff)
    => (FreeModel ops ~> ops')
    -> ops (FreeT ops' eff)
