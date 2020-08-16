module Casimir.Ops.State.StateT where

import Casimir.Base
import Casimir.Ops.State.Effect

import Data.Tuple (swap)
import Control.Monad.Trans.State (StateT (..))

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as StateT

stateOps :: forall s m . (Monad m) => StateOps s (StateT s m)
stateOps = StateOps
  { getOp = StateT.get
  , putOp = StateT.put
  }

contraLift
  :: forall m s
   . (Monad m)
  => ContraLift m (StateT s m)
contraLift = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . ((forall x . StateT s m x -> m (s, x))
        -> m (s, a))
    -> StateT s m a
  contraLift1 cont1 = do
    s1 <- StateT.get
    let
      contraLift2 :: forall x . StateT s m x -> m (s, x)
      contraLift2 comp = swap <$> runStateT comp s1
    (s2, x) <- Trans.lift $ cont1 contraLift2
    StateT.put s2
    return x

lift
  :: forall s m . (Monad m)
  => Lift m (StateT s m)
lift = Lift Trans.lift

higherLift
  :: forall s m . (Monad m)
  => HigherLift m (StateT s m)
higherLift =
  HigherLift Trans.lift contraLift
