
module Casimir.Free.Handler
  ( withCoOpHandler
  , withCoOpHandlerAndOps
  , withContextualCoOpHandler
  )
where

import Casimir.Base

import Casimir.Free.FreeEff
import Casimir.Free.FreeOps

{-# INLINE withCoOpHandler #-}
withCoOpHandler
  :: forall free handler m a r
   . ( Monad m
     , EffOps handler
     , FreeOps handler
     , FreeHandler free
     , ImplicitOps handler
     )
  => CoOpHandler handler a r m
  -> ((OpsConstraint handler (free handler m))
      => free handler m a)
  -> m r
withCoOpHandler handler comp1
  = handleFree @free handler
      $ withOps (freeOps @free @handler @m) comp1

{-# INLINE withCoOpHandlerAndOps #-}
withCoOpHandlerAndOps
  :: forall free ops handler m a r
    . ( EffOps ops
      , EffOps handler
      , FreeOps handler
      , ImplicitOps ops
      , FreeHandler free
      , ImplicitOps handler
      , EffConstraint ops m
      , EffFunctor Lift (Operation ops)
      )
  => CoOpHandler handler a r m
  -> (( OpsConstraint handler (free handler m)
      , OpsConstraint ops (free handler m)
      )
      => free handler m a)
  -> m r
withCoOpHandlerAndOps handler comp1
  = handleFree @free handler $
      withOps
        ( freeOps @free @handler @m
        ∪ effmap (Lift $ liftFree @free) (captureOps @ops)
        )
        comp1

{-# INLINE withContextualCoOpHandler #-}
withContextualCoOpHandler
  :: forall free handler m a r
   . ( Monad m
     , EffOps handler
     , FreeOps handler
     , FreeHandler free
     , ImplicitOps handler
     )
  => CoOpHandler handler a r m
  -> (r -> m a)
  -> ((OpsConstraint handler (free handler m))
      => free handler m a)
  -> m a
withContextualCoOpHandler handler extract comp
  = withCoOpHandler @free handler comp >>= extract
