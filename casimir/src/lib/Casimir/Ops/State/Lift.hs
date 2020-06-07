module Casimir.Ops.State.Lift
where

import Data.Tuple (swap)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.State.Class (MonadState  (..))
import Control.Monad.Trans.State.Strict (StateT, runStateT)

import Casimir.Base
import Casimir.Transform

liftStateT
  :: forall s eff a
   . (Monad eff)
  => eff a
  -> StateT s eff a
liftStateT = lift

stateTLift
  :: forall s eff . (Monad eff)
  => Lift eff (StateT s eff)
stateTLift = Lift liftStateT

stateTContraLift
  :: forall eff s
   . (Monad eff)
  => ContraLift eff (StateT s eff)
stateTContraLift = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . ((forall x . StateT s eff x -> eff (s, x))
        -> eff (s, a))
    -> StateT s eff a
  contraLift1 cont1 = do
    s1 <- get
    let
      contraLift2 :: forall x . StateT s eff x -> eff (s, x)
      contraLift2 comp = swap <$> runStateT comp s1
    (s2, x) <- lift $ cont1 contraLift2
    put s2
    return x

-- Show that StateT contra lift can be derived from its
-- MonadTransControl instance
stateTContraLift'
  :: forall eff s
   . (Monad eff)
  => ContraLift eff (StateT s eff)
stateTContraLift' = transformContraLift
  @eff @(StateT s) @((,) s)
  swap swap

stateTHigherLift
  :: forall s eff . (Monad eff)
  => HigherLift eff (StateT s eff)
stateTHigherLift =
  HigherLift liftStateT stateTContraLift
