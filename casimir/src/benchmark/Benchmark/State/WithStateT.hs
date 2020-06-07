
module Benchmark.State.WithStateT
where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)

import qualified Control.Monad.Trans.Reader as RT

import Casimir
import Casimir.Ops.State.Transform

import Benchmark.State.Base

withStateOpsComp :: forall m . (Monad m)
  => StateT Int m ()
withStateOpsComp = withOps stateTOps stateBaseFunc

withStateTHandlerComp :: forall m . (Monad m)
  => StateT Int m ()
withStateTHandlerComp = withOpsHandler stateTHandler stateBaseFunc

withStateTReaderTComp :: forall m . (Monad m)
  => ReaderT Int m ()
withStateTReaderTComp = do
  s <- RT.ask
  lift $ evalStateT withStateTHandlerComp s
