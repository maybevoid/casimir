{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.FreeEff
where

import Control.Natural (type (~>))
import Control.Monad.Trans.Free (FreeT)

import Control.Effect.Base.Effect

class FreeEff (ops :: *) where
  type family Operation ops = (f :: (* -> *) -> *) | f -> ops
  type family CoOperation ops = (f :: (* -> *)) | f -> ops

  freeMonad
    :: forall ops' eff .
    (Functor ops', Effect eff)
    => (CoOperation ops ~> ops')
    -> Operation ops (FreeT ops' eff)

  -- freeMonad
  --   :: forall ops2 eff .
  --   ( Functor (Operation ops2 eff)
  --   , Effect eff
  --   )
  --   => (CoOperation ops ~> Operation ops2 eff)
  --   -> Operation ops (FreeT (Operation ops2 eff) eff)

type Model ops = CoOperation ops