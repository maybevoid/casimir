
module Benchmark.State.Codensity
where

import Casimir
import Casimir.Freer
import Casimir.Ops.State
import Casimir.Ops.State.Freer

import Benchmark.State.Base

codensityComp
    :: forall eff
     . (Effect eff)
    => Int
    -> eff ()
codensityComp s = comp2
 where
  handleOps
    :: forall x r
     . CoOperation (StateEff Int) x
    -> (x -> CoState Int eff r)
    -> CoState Int eff r
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

  handleReturn :: a -> CoState s eff a
  handleReturn x = CoState $ \_ -> return x
  {-# INLINABLE handleReturn #-}

  ops :: Operation (StateEff Int) (Codensity (CoState Int eff))
  ops = codensityOps handleOps

  comp1 :: Codensity (CoState Int eff) ()
  comp1 = withOps ops stateBaseFunc

  comp2 :: eff ()
  comp2 = runCoState s $ runCodensity comp1 handleReturn
{-# INLINABLE codensityComp #-}
