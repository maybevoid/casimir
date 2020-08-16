module Casimir.Ops.State.Effect where

import Casimir.Base

data StateTag

data StateOps s m = StateOps
  { getOp :: m s
  , putOp :: s -> m ()
  }

instance HasLabel (StateOps s) where
  type GetLabel (StateOps s) = StateTag

get :: forall s . Eff1 (StateOps s) s
get = withCaptureOps getOp

put :: forall s . s -> Eff1 (StateOps s) ()
put s = withCaptureOps $ \ops -> putOp ops s

instance EffFunctor Lift (StateOps s) where
  effmap (Lift lift) ops = StateOps
    { getOp = lift $ getOp ops
    , putOp = lift <$> putOp ops
    }
