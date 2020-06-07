
module Casimir.Freer.Handler
  ( withCoOpHandler
  )
where

import Casimir.Base

import Casimir.Freer.FreeOps
import Casimir.Freer.FreeEff

{-# INLINE withCoOpHandler #-}
withCoOpHandler
  :: forall free handler m a r
   . ( Monad m
     , EffOps handler
     , FreeOps handler
     , ImplicitOps handler
     , FreeEff free
     )
  => CoOpHandler handler a r m
  -> ((OpsConstraint handler (free handler m))
      => free handler m a)
  -> m r
withCoOpHandler handler comp1
  = handleFree @free handler $
      withOps (freeOps @free @handler @m) comp1
