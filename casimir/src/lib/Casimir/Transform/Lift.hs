
module Casimir.Transform.Lift
where

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Monad.Trans.Control

import Casimir.Base

transformLift
  :: forall t eff
   . ( Effect eff
     , MonadTrans t
     , Effect (t eff)
     )
  => Lift eff (t eff)
transformLift = Lift lift

transformContraLift
  :: forall eff t w
   . ( Effect eff
     , Effect (t eff)
     , MonadTransControl t
     , Functor w
     )
  => (forall x . StT t x -> w x)
  -> (forall x . w x -> StT t x)
  -> ContraLift eff (t eff)
transformContraLift id1 id2 = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . ((forall x . t eff x -> eff (w x))
        -> eff (w a))
    -> t eff a
  contraLift1 cont1 = liftWith cont2 >>= restoreT . return
   where
    cont2
      :: (forall x . t eff x -> eff (StT t x))
      -> eff (StT t a)
    cont2 contraLift2 = do
      wx <- cont1 contraLift3
      return $ id2 wx
     where
      contraLift3
        :: forall x . t eff x -> eff (w x)
      contraLift3 mx = do
        wx <- contraLift2 mx
        return $ id1 wx

baseContraLift
  :: forall eff1 eff2 w
   . ( Effect eff1
     , Effect eff2
     , Functor w
     , MonadBaseControl eff1 eff2
     )
  => (forall x . StM eff2 x -> w x)
  -> (forall x . w x -> StM eff2 x)
  -> ContraLift eff1 eff2
baseContraLift id1 id2 = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . ((forall x . eff2 x -> eff1 (w x))
        -> eff1 (w a))
    -> eff2 a
  contraLift1 cont1 = control cont2
   where
    cont2
      :: (forall x . eff2 x -> eff1 (StM eff2 x))
      -> eff1 (StM eff2 a)
    cont2 contraLift2 = do
      wx <- cont1 contraLift3
      return $ id2 wx
     where
      contraLift3
        :: forall x . eff2 x -> eff1 (w x)
      contraLift3 mx = do
        wx <- contraLift2 mx
        return $ id1 wx
