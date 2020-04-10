
module Benchmark.State.MTL
where

import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Trans.State.Strict (StateT)

stateMTLFunc
  :: forall eff
   . (Monad eff)
  => StateT Int eff ()
stateMTLFunc =
 do
  s <- get
  if s <= 0
    then return ()
    else do
      put $ s - 1
      stateMTLFunc
