
module Control.Effect.Implicit.Free.Handler
  ( withCoOpHandler
  , withCoOpHandlerAndOps
  , withContextualCoOpHandler
  )
where

import Control.Effect.Implicit.Base

import Control.Effect.Implicit.Free.FreeEff
import Control.Effect.Implicit.Free.FreeOps

{-# INLINE withCoOpHandler #-}
withCoOpHandler
  :: forall free handler eff a r
   . ( Effect eff
     , EffOps handler
     , FreeOps handler
     , FreeHandler free
     , ImplicitOps handler
     )
  => CoOpHandler handler a r eff
  -> ((OpsConstraint handler (free handler eff))
      => free handler eff a)
  -> eff r
withCoOpHandler handler comp1
  = handleFree @free handler
      $ withOps (freeOps @free @handler @eff) comp1

{-# INLINE withCoOpHandlerAndOps #-}
withCoOpHandlerAndOps
  :: forall free ops handler eff a r
    . ( EffOps ops
      , EffOps handler
      , FreeOps handler
      , ImplicitOps ops
      , FreeHandler free
      , ImplicitOps handler
      , EffConstraint ops eff
      , EffFunctor (Operation ops)
      )
  => CoOpHandler handler a r eff
  -> (( OpsConstraint handler (free handler eff)
      , OpsConstraint ops (free handler eff)
      )
      => free handler eff a)
  -> eff r
withCoOpHandlerAndOps handler comp1
  = handleFree @free handler $
      withOps
        ( freeOps @free @handler @eff
        ∪ effmap (liftFree @free) (captureOps @ops)
        )
        comp1

{-# INLINE withContextualCoOpHandler #-}
withContextualCoOpHandler
  :: forall free handler eff a r
   . ( Effect eff
     , EffOps handler
     , FreeOps handler
     , FreeHandler free
     , ImplicitOps handler
     )
  => CoOpHandler handler a r eff
  -> (r -> eff a)
  -> ((OpsConstraint handler (free handler eff))
      => free handler eff a)
  -> eff a
withContextualCoOpHandler handler extract comp
  = withCoOpHandler @free handler comp >>= extract
