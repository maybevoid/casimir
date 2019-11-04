
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
