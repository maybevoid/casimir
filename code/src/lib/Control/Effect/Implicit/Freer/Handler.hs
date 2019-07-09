
module Control.Effect.Implicit.Freer.Handler
  ( withCoOpHandler
  )
where

import Control.Effect.Implicit.Base

import Control.Effect.Implicit.Freer.FreeOps
import Control.Effect.Implicit.Freer.FreeEff

{-# INLINE withCoOpHandler #-}
withCoOpHandler
  :: forall free handler eff a r
   . ( Effect eff
     , FreeOps handler
     , ImplicitOps handler
     , FreeEff free
     )
  => FreerCoOpHandler handler a r eff
  -> ((OpsConstraint handler (free handler eff))
      => free handler eff a)
  -> eff r
withCoOpHandler handler comp1
  = handleFree @free handler $
      withOps (freeOps @free @handler @eff) comp1
