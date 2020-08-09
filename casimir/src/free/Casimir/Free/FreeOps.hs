{-# LANGUAGE PolyKinds #-}

module Casimir.Free.FreeOps
  ( FreeOps (..)
  )
where

import Data.Kind
import Casimir.Base

class
  ( Effects ops
  , Functor (CoOperation ops)
  )
  => FreeOps (ops :: (Type -> Type) -> Type) where
    type family CoOperation ops :: (Type -> Type)

    mkFreeOps
      :: forall m
      . (Monad m)
      => (forall a . CoOperation ops a -> m a)
      -> ops m
