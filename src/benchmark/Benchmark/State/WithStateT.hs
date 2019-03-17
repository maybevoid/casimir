
module Benchmark.State.WithStateT
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import qualified Control.Monad.Trans.Reader as RT

import Control.Effect
import Benchmark.State.Base

stateTComp1 :: forall eff . (Effect eff)
  => StateT Int eff ()
stateTComp1 = withHandler stateTHandler stateBaseFunc

withStateTComp :: forall eff . (Effect eff)
  => ReaderT Int eff ()
withStateTComp = do
  s <- RT.ask
  lift $ evalStateT stateTComp1 s
