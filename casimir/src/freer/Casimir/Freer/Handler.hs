
module Casimir.Freer.Handler
  ( withCoOpHandler
  )
where

import Casimir.Base

import Casimir.Freer.FreeOps
import Casimir.Freer.FreeTransformer

{-# INLINE withCoOpHandler #-}
withCoOpHandler
  :: forall free eff ops m a r
   . ( Monad m
     , Effects eff
     , FreeOps ops
     , ImplicitOps eff
     , FreeTransformer free
     , Operations eff ~ ops
     )
  => CoOpHandler ops a r m
  -> ((OpsConstraint eff (free ops m))
      => free ops m a)
  -> m r
withCoOpHandler handler comp1
  = handleFree @free handler $
      withOps (freeOps @free @ops @m) comp1
