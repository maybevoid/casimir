{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Effect.Base.FreeOps
where

import Data.Kind
import Control.Effect.Base.Effect
import Control.Effect.Base.EffFunctor

class
  ( Functor (CoOperation ops)
  , EffFunctor (Operation ops)
  )
  => FreeOps (ops :: Type) where
    type family Operation ops
      = (f :: (Type -> Type) -> Type) | f -> ops

    type family CoOperation ops
      = (f :: (Type -> Type)) | f -> ops

    mkFreeOps
      :: forall t eff
      . ( Effect eff
        , Effect (t eff)
        )
      => (forall a . (CoOperation ops a) -> t eff a)
      -> Operation ops (t eff)
