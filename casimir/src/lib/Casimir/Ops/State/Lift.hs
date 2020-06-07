module Casimir.Ops.State.Lift
where

import Data.Tuple (swap)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.State.Class (MonadState  (..))
import Control.Monad.Trans.State.Strict (StateT, runStateT)

import Casimir.Base
import Casimir.Transform

liftStateT
  :: forall s m a
   . (Monad m)
  => m a
  -> StateT s m a
liftStateT = lift

stateTLift
  :: forall s m . (Monad m)
  => Lift m (StateT s m)
stateTLift = Lift liftStateT

stateTContraLift
  :: forall m s
   . (Monad m)
  => ContraLift m (StateT s m)
stateTContraLift = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . ((forall x . StateT s m x -> m (s, x))
        -> m (s, a))
    -> StateT s m a
  contraLift1 cont1 = do
    s1 <- get
    let
      contraLift2 :: forall x . StateT s m x -> m (s, x)
      contraLift2 comp = swap <$> runStateT comp s1
    (s2, x) <- lift $ cont1 contraLift2
    put s2
    return x

-- Show that StateT contra lift can be derived from its
-- MonadTransControl instance
stateTContraLift'
  :: forall m s
   . (Monad m)
  => ContraLift m (StateT s m)
stateTContraLift' = transformContraLift
  @m @(StateT s) @((,) s)
  swap swap

stateTHigherLift
  :: forall s m . (Monad m)
  => HigherLift m (StateT s m)
stateTHigherLift =
  HigherLift liftStateT stateTContraLift
