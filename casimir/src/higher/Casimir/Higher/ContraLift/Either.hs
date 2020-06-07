
module Casimir.Higher.ContraLift.Either
where

import Casimir.Higher.ContraLift

contraEither
  :: forall m e
   . (Monad m)
  => ContraFree m (Either e)
contraEither = handler1
 where
  handler1
    :: forall a
     . ((forall x . Either e (m x) -> m (Either e x))
        -> m (Either e (m (Either e a))))
    -> m (Either e a)
  handler1 cont1 = do
    res <- cont1 contra1
    case res of
      (Left e) -> return $ Left e
      (Right mx) -> mx

  contra1 :: forall a . Either e (m a) -> m (Either e a)
  contra1 (Left e) = return $ Left e
  contra1 (Right mx) = mx >>= return . Right
