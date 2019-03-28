
module Benchmark.State.Base
where

import Control.Effect.Implicit
import Control.Effect.Implicit.Ops.State

newtype CoState s eff a = CoState (s -> eff a)

stateBaseFunc
  :: forall eff
   . (EffConstraint (StateEff Int) eff)
  => eff ()
stateBaseFunc =
 do
  s <- get
  if s <= 0
    then return ()
    else do
      put $ s - 1
      stateBaseFunc
{-# INLINE stateBaseFunc #-}

stateBaseComp :: GenericReturn (StateEff Int) ()
stateBaseComp = genericReturn stateBaseFunc

runCoState
  :: forall s eff . (Effect eff)
  => s
  -> (forall a . CoState s eff a -> eff a)
runCoState i (CoState cont) = cont i
{-# INLINE runCoState #-}

stateCoOpHandler
  :: forall eff s a .
  (Effect eff)
  => CoOpHandler (StateEff s) a (CoState s eff a) eff
stateCoOpHandler = CoOpHandler handleReturn handleOps
 where
  handleReturn :: a -> eff (CoState s eff a)
  handleReturn x = return $ CoState $ \_ -> return x
  {-# INLINE handleReturn #-}

  handleOps :: StateCoOp s (eff (CoState s eff a)) -> eff (CoState s eff a)
  handleOps (GetOp cont1) = return $ CoState $
    \s ->
     do
      (CoState cont2) <- cont1 s
      cont2 s
  handleOps (PutOp s cont1) = return $ CoState $
    \_ ->
     do
      (CoState cont2) <- cont1 ()
      cont2 s
  {-# INLINE handleOps #-}
{-# INLINE stateCoOpHandler #-}

stateFreerCoOpHandler
  :: forall eff s a .
  (Effect eff)
  => FreerCoOpHandler (StateEff s) a (CoState s eff a) eff
stateFreerCoOpHandler = FreerCoOpHandler handleReturn handleOps
 where
  handleReturn :: a -> eff (CoState s eff a)
  handleReturn x = return $ CoState $ \_ -> return x
  {-# INLINE handleReturn #-}

  handleOps
    :: forall x
     . StateCoOp s x
    -> (x -> eff (CoState s eff a))
    -> eff (CoState s eff a)
  handleOps (GetOp cont1) cont2 = return $ CoState $
    \s ->
     do
      (CoState cont3) <- cont2 $ cont1 s
      cont3 s
  handleOps (PutOp s cont1) cont2 = return $ CoState $
    \_ ->
     do
      (CoState cont3) <- cont2 $ cont1 ()
      cont3 s
  {-# INLINE handleOps #-}
{-# INLINE stateFreerCoOpHandler #-}
