
module Control.Effect.Computation.Free
where

import Control.Effect.Base
import Control.Monad.Trans.Free

handleFree
  :: forall ops eff a r
   . (Effect eff, EffOps ops)
  => OpsHandler ops a r eff
  -> FreeT (CoOperation ops) eff a
  -> eff r
handleFree handler = handleFree'
 where
  handleFree'
   :: FreeT (CoOperation ops) eff a
    -> eff r
  handleFree' comp = runFreeT comp >>= handleComp

  handleComp
    :: FreeF (CoOperation ops) a (FreeT (CoOperation ops) eff a)
    -> eff r
  handleComp (Pure x) = handleReturn handler x
  handleComp (Free ops) = handleOps handler $ fmap handleFree' ops