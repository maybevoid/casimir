
module Benchmark.State.Codensity
where

import Casimir
import Casimir.Freer
import Casimir.Ops.State
import Casimir.Ops.State.Freer

import Benchmark.State.Base

codensityComp
    :: forall m
     . (Monad m)
    => Int
    -> m ()
codensityComp s = comp2
 where
  handleOps
    :: forall x r
     . CoOperation (State Int) x
    -> (x -> CoState Int m r)
    -> CoState Int m r
  handleOps GetOp cont1 = CoState $
    \s' ->
      let
        (CoState cont3) = cont1 s'
      in cont3 s'
  handleOps (PutOp s') cont1 = CoState $
    \_ ->
      let
        (CoState cont3) = cont1 ()
      in cont3 s'
  {-# INLINABLE handleOps #-}

  handleReturn :: a -> CoState s m a
  handleReturn x = CoState $ \_ -> return x
  {-# INLINABLE handleReturn #-}

  ops :: Operations (State Int) (Codensity (CoState Int m))
  ops = codensityOps handleOps

  comp1 :: Codensity (CoState Int m) ()
  comp1 = withOps ops stateBaseFunc

  comp2 :: m ()
  comp2 = runCoState s $ runCodensity comp1 handleReturn
{-# INLINABLE codensityComp #-}
