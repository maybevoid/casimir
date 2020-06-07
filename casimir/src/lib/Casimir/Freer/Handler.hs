
module Casimir.Freer.Handler
  ( withCoOpHandler
  )
where

import Casimir.Base

import Casimir.Freer.FreeOps
import Casimir.Freer.FreeEff

{-# INLINE withCoOpHandler #-}
withCoOpHandler
  :: forall free handler eff a r
   . ( Monad eff
     , EffOps handler
     , FreeOps handler
     , ImplicitOps handler
     , FreeEff free
     )
  => CoOpHandler handler a r eff
  -> ((OpsConstraint handler (free handler eff))
      => free handler eff a)
  -> eff r
withCoOpHandler handler comp1
  = handleFree @free handler $
      withOps (freeOps @free @handler @eff) comp1
