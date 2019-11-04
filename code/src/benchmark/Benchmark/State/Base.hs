
module Benchmark.State.Base
where

import Control.Effect.Implicit
import Control.Effect.Implicit.Ops.State

import qualified Control.Effect.Implicit.Free as Free
import qualified Control.Effect.Implicit.Freer as Freer

newtype CoState s eff a = CoState (s -> eff a)

stateBaseFunc :: Eff (StateEff Int) ()
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
  => Free.CoOpHandler (StateEff s) a (CoState s eff a) eff
stateCoOpHandler = Free.CoOpHandler handleReturn handleOps
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
  => Freer.CoOpHandler (StateEff s) a (CoState s eff a) eff
stateFreerCoOpHandler = Freer.CoOpHandler handleReturn handleOps
 where
  handleReturn :: a -> eff (CoState s eff a)
  handleReturn x = return $ CoState $ \_ -> return x
  {-# INLINE handleReturn #-}

  handleOps
    :: forall x
     . Freer.CoOperation (StateEff s) x
    -> (x -> eff (CoState s eff a))
    -> eff (CoState s eff a)
  handleOps GetOp' cont1 = return $ CoState $
    \s ->
     do
      (CoState cont3) <- cont1 s
      cont3 s
  handleOps (PutOp' s) cont1 = return $ CoState $
    \_ ->
     do
      (CoState cont3) <- cont1 ()
      cont3 s
  {-# INLINE handleOps #-}
{-# INLINE stateFreerCoOpHandler #-}
