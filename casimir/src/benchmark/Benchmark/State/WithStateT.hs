
module Benchmark.State.WithStateT
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import qualified Control.Monad.Trans.Reader as RT

import Casimir
import Casimir.Ops.State.Transform

import Benchmark.State.Base

withStateOpsComp :: forall eff . (Monad eff)
  => StateT Int eff ()
withStateOpsComp = withOps stateTOps stateBaseFunc

withStateTHandlerComp :: forall eff . (Monad eff)
  => StateT Int eff ()
withStateTHandlerComp = withOpsHandler stateTHandler stateBaseFunc

withStateTReaderTComp :: forall eff . (Monad eff)
  => ReaderT Int eff ()
withStateTReaderTComp = do
  s <- RT.ask
  lift $ evalStateT withStateTHandlerComp s
