
module Casimir.Freer.FreeOps
  ( FreeOps (..)
  )
where

import Casimir.Base
import Casimir.Freer.CoOp

class
  ( Effects ops
  , EffCoOp ops
  )
  => FreeOps ops
   where
    mkFreeOps
      :: forall m
        . (Monad m)
      => (forall a . CoOperation ops a -> m a)
      -> Operations ops m
