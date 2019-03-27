
module Control.Effect.Implicit.Computation.Free
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation.Class
import Control.Effect.Implicit.Computation.Value
import Control.Effect.Implicit.Computation.Pipeline

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

{-# INLINE coopHandlerToPipeline #-}
coopHandlerToPipeline
  :: forall free ops1 handler eff1 a b .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  , FreeEff free
  )
  => Computation ops1 (CoOpHandler handler a b) eff1
  -> Pipeline ops1 handler eff1 eff1 (Return a) (Return b)
coopHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline
    :: forall ops2 .
    (EffOps ops2)
    => Computation (handler ∪ ops2) (Return a) eff1
    -> Computation (ops1 ∪ ops2) (Return b) eff1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall eff2 .
      (Effect eff2)
      => LiftEff eff1 eff2
      -> Operation (ops1 ∪ ops2) eff2
      -> Return b eff2
    comp2 lift12 (UnionOps ops1 ops2) = Return comp4
     where
      handler2 :: CoOpHandler handler a b eff2
      handler2 = runComp handler1 lift12 ops1

      comp3 :: free handler eff2 a
      comp3 = returnVal $ runComp comp1
        (joinLift lift12 freeLiftEff)
        (freeOps ∪ (effmap liftFree ops2))

      comp4 :: eff2 b
      comp4 = handleFree handler2 comp3

{-# INLINE genericCoOpHandlerToPipeline #-}
genericCoOpHandlerToPipeline
  :: forall free ops1 handler eff1 .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  , FreeEff free
  )
  => Computation ops1 (GenericCoOpHandler handler) eff1
  -> GenericPipeline ops1 handler eff1
genericCoOpHandlerToPipeline handler1
  = transformerPipeline $ Computation handler2
 where
  handler2
    :: forall eff2 .
    (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation ops1 eff2
    -> TransformerHandler (free handler) handler eff2
  handler2 lift12 ops1
    = TransformerHandler freeOps freeLiftEff (mkLiftEff unliftFree)
    where
      (GenericCoOpHandler handler3) = runComp handler1 lift12 ops1

      unliftFree
        :: forall a .
        free handler eff2 a
        -> eff2 a
      unliftFree = handleFree handler3

{-# INLINE contextualHandlerToPipeline #-}
contextualHandlerToPipeline
  :: forall free w ops1 handler eff1 .
  ( Effect eff1
  , EffOps ops1
  , EffOps handler
  , FreeEff free
  )
  => Computation ops1 (ContextualHandler w handler) eff1
  -> GenericPipeline ops1 handler eff1
contextualHandlerToPipeline handler1
  = transformerPipeline $ Computation handler2
 where
  handler2
    :: forall eff2 .
    (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation ops1 eff2
    -> TransformerHandler (free handler) handler eff2
  handler2 lift12 ops1
    = TransformerHandler freeOps freeLiftEff (mkLiftEff unliftFree)
   where
    (ContextualHandler handler3 extract) = runComp handler1 lift12 ops1

    unliftFree
      :: forall a .
      free handler eff2 a
      -> eff2 a
    unliftFree eff = do
      wx <- handleFree handler3 eff
      extract wx
