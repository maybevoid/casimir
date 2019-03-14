{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.FreeOps
where

import Control.Effect.Base.Effect
import Control.Effect.Base.EffFunctor

class
  ( Functor (CoOperation ops)
  , EffFunctor (Operation ops)
  )
  => FreeOps (ops :: *) where
    type family Operation ops
      = (f :: (* -> *) -> *) | f -> ops

    type family CoOperation ops
      = (f :: (* -> *)) | f -> ops

    mkFreeOps
      :: forall t eff
      . ( Effect eff
        , Effect (t eff)
        )
      => (forall a . (CoOperation ops a) -> t eff a)
      -> Operation ops (t eff)
