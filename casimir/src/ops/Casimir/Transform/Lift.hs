
module Casimir.Transform.Lift
where

import Control.Monad.Trans.Class
  (MonadTrans (..))

import Control.Monad.Trans.Control

import Casimir.Base

transformLift
  :: forall t m
   . ( Monad m
     , MonadTrans t
     , Monad (t m)
     )
  => Lift m (t m)
transformLift = Lift lift

transformContraLift
  :: forall m t w
   . ( Monad m
     , Monad (t m)
     , MonadTransControl t
     , Functor w
     )
  => (forall x . StT t x -> w x)
  -> (forall x . w x -> StT t x)
  -> ContraLift m (t m)
transformContraLift id1 id2 = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . ((forall x . t m x -> m (w x))
        -> m (w a))
    -> t m a
  contraLift1 cont1 = liftWith cont2 >>= restoreT . return
   where
    cont2
      :: (forall x . t m x -> m (StT t x))
      -> m (StT t a)
    cont2 contraLift2 = do
      wx <- cont1 contraLift3
      return $ id2 wx
     where
      contraLift3
        :: forall x . t m x -> m (w x)
      contraLift3 mx = do
        wx <- contraLift2 mx
        return $ id1 wx

baseContraLift
  :: forall m1 m2 w
   . ( Monad m1
     , Monad m2
     , Functor w
     , MonadBaseControl m1 m2
     )
  => (forall x . StM m2 x -> w x)
  -> (forall x . w x -> StM m2 x)
  -> ContraLift m1 m2
baseContraLift id1 id2 = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . ((forall x . m2 x -> m1 (w x))
        -> m1 (w a))
    -> m2 a
  contraLift1 cont1 = control cont2
   where
    cont2
      :: (forall x . m2 x -> m1 (StM m2 x))
      -> m1 (StM m2 a)
    cont2 contraLift2 = do
      wx <- cont1 contraLift3
      return $ id2 wx
     where
      contraLift3
        :: forall x . m2 x -> m1 (w x)
      contraLift3 mx = do
        wx <- contraLift2 mx
        return $ id1 wx
