{-# Language PolyKinds #-}

module Casimir.Freer.Handler
  ( withCoOpHandler
  )
where

import Casimir.Base

import Casimir.Freer.FreeOps
import Casimir.Freer.FreeTransformer

{-# INLINE withCoOpHandler #-}
withCoOpHandler
  :: forall free ops m a r
   . ( Monad m
     , Effects ops
     , FreeOps ops
     , FreeTransformer free
     )
  => CoOpHandler ops a r m
  -> ((OpsConstraint ops (free ops m))
      => free ops m a)
  -> m r
withCoOpHandler handler comp1
  = handleFree @free handler $
      withOps (freeOps @free @ops @m) comp1
