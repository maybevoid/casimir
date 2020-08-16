module Casimir.Ops.Resource.Higher.Effect where

import Casimir.Higher

import qualified Control.Monad.Trans.Reader as ReaderT

data ResourceTag

data ResourceOps s (m :: MonadPair) = ResourceOps
  { withResourceOp
      :: forall a b
       . s a
      -> (a -> M1 m b)
      -> M2 m b
  }

instance HasLabel (ResourceOps s) where
  type GetLabel (ResourceOps s) = ResourceTag

withResource
  :: forall ops m s a b
   . ( Monad m
     , ParamConstraint (Params '[ResourceOps s]) ops
     )
  => s a
  -> (a -> Eff' ops m b)
  -> Eff' ops m b
withResource resource cont = do
  ops1 <- ReaderT.ask
  (ResourceOps ops2) <- captureOps @(ResourceOps s)
  eff $ ops2 resource $ withOps ops1 <$> cont
