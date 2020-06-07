
module Benchmark.State.HandleFree
where

import qualified Casimir.Free as Free

import qualified Casimir.Freer as Freer

import Benchmark.State.Base

handleFreeComp
  :: forall free m
   . (Free.FreeHandler free, Monad m)
  => Int
  -> m ()
handleFreeComp s
  = Free.withContextualCoOpHandler @free
    stateCoOpHandler
    (runCoState s)
    stateBaseFunc

handleFreerComp
  :: forall free m
   . (Freer.FreeEff free, Monad m)
  => Int
  -> m ()
handleFreerComp s =
  Freer.withCoOpHandler @free
    stateFreerCoOpHandler
    stateBaseFunc
  >>= runCoState s
