
module Benchmark.State.Base
where

import Control.Monad.Trans.State.Strict (StateT)
import Control.Monad.Reader (ReaderT (..))


import Casimir
import Casimir.Ops.Env
import Casimir.Ops.Env.Transform
import Casimir.Ops.State
import Casimir.Ops.State.Transform

import qualified Casimir.Ops.State.Free as Free
import qualified Casimir.Ops.State.Freer as Freer

import qualified Casimir.Free as Free
import qualified Casimir.Freer as Freer

newtype CoState s m a = CoState (s -> m a)

stateBaseFunc :: Eff (State Int) ()
stateBaseFunc =
 do
  s <- get
  if s <= 0
    then return ()
    else do
      put $ s - 1
      stateBaseFunc
{-# INLINE stateBaseFunc #-}

stateBaseComp :: forall m . (Monad m)
  => BaseComputation (State Int) (Return ()) m
stateBaseComp = genericReturn stateBaseFunc

readerTHandler
  :: forall a m .
  (Monad m)
  => BaseOpsHandler NoEff (EnvEff a) (ReaderT a m)
readerTHandler = opsHandlerComp $
  \lifter -> effmap lifter readerTOps

stateTHandler
  :: forall m s .
  (Monad m)
  => BaseOpsHandler NoEff (State s) (StateT s m)
stateTHandler = opsHandlerComp $
  \lifter -> effmap lifter stateTOps

runCoState
  :: forall s m . (Monad m)
  => s
  -> (forall a . CoState s m a -> m a)
runCoState i (CoState cont) = cont i
{-# INLINE runCoState #-}

stateCoOpHandler
  :: forall m s a .
  (Monad m)
  => Free.CoOpHandler (State s) a (CoState s m a) m
stateCoOpHandler = Free.CoOpHandler handleReturn handleOps
 where
  handleReturn :: a -> m (CoState s m a)
  handleReturn x = return $ CoState $ \_ -> return x
  {-# INLINE handleReturn #-}

  handleOps :: Free.StateCoOp s (m (CoState s m a)) -> m (CoState s m a)
  handleOps (Free.GetOp cont1) = return $ CoState $
    \s ->
     do
      (CoState cont2) <- cont1 s
      cont2 s
  handleOps (Free.PutOp s cont1) = return $ CoState $
    \_ ->
     do
      (CoState cont2) <- cont1 ()
      cont2 s
  {-# INLINE handleOps #-}
{-# INLINE stateCoOpHandler #-}

stateFreerCoOpHandler
  :: forall m s a .
  (Monad m)
  => Freer.CoOpHandler (State s) a (CoState s m a) m
stateFreerCoOpHandler = Freer.CoOpHandler handleReturn handleOps
 where
  handleReturn :: a -> m (CoState s m a)
  handleReturn x = return $ CoState $ \_ -> return x
  {-# INLINE handleReturn #-}

  handleOps
    :: forall x
     . Freer.CoOperation (State s) x
    -> (x -> m (CoState s m a))
    -> m (CoState s m a)
  handleOps Freer.GetOp cont1 = return $ CoState $
    \s ->
     do
      (CoState cont3) <- cont1 s
      cont3 s
  handleOps (Freer.PutOp s) cont1 = return $ CoState $
    \_ ->
     do
      (CoState cont3) <- cont1 ()
      cont3 s
  {-# INLINE handleOps #-}
{-# INLINE stateFreerCoOpHandler #-}
