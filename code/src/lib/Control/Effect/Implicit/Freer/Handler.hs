
module Control.Effect.Implicit.Freer.Handler
  ( withFreerCoOpHandler
  )
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Freer.FreerEff

{-# INLINE withFreerCoOpHandler #-}
withFreerCoOpHandler
  :: forall free handler eff a r
   . ( Effect eff
     , FreerOps handler
     , ImplicitOps handler
     , FreerEff free
     )
  => FreerCoOpHandler handler a r eff
  -> ((OpsConstraint handler (free handler eff))
      => free handler eff a)
  -> eff r
withFreerCoOpHandler handler comp1
  = handleFreer @free handler $
      withOps (freerOps @free @handler @eff) comp1
