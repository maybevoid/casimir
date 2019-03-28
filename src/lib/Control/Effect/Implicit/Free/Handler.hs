
module Control.Effect.Implicit.Free.Handler
  ( withCoOpHandler
  , withFreerCoOpHandler
  , withContextualCoOpHandler
  )
where

import Control.Effect.Implicit.Base

import Control.Effect.Implicit.Free.FreeEff

{-# INLINE withCoOpHandler #-}
withCoOpHandler
  :: forall free ops eff a r
   . ( Effect eff
     , EffOps ops
     , FreeEff free
     )
  => CoOpHandler ops a r eff
  -> ((OpsConstraint ops (free ops eff))
      => free ops eff a)
  -> eff r
withCoOpHandler handler comp1
  = handleFree @free handler
      $ withOps (freeOps @free @ops @eff) comp1

{-# INLINE withFreerCoOpHandler #-}
withFreerCoOpHandler
  :: forall free ops eff a r
   . ( Effect eff
     , EffOps ops
     , FreerEff free
     )
  => FreerCoOpHandler ops a r eff
  -> ((OpsConstraint ops (free ops eff))
      => free ops eff a)
  -> eff r
withFreerCoOpHandler handler comp1
  = handleFreer @free handler $
      withOps (freeOps @free @ops @eff) comp1

{-# INLINE withContextualCoOpHandler #-}
withContextualCoOpHandler
  :: forall free ops eff a r
   . ( Effect eff
     , EffOps ops
     , FreeEff free
     )
  => CoOpHandler ops a r eff
  -> (r -> eff a)
  -> ((OpsConstraint ops (free ops eff))
      => free ops eff a)
  -> eff a
withContextualCoOpHandler handler extract comp
  = withCoOpHandler @free handler comp >>= extract
