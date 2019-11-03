
module Control.Effect.Implicit.Higher.ContraLift.Either
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.ContraLift

contraEither
  :: forall eff e
   . (Effect eff)
  => ContraFree eff (Either e)
contraEither = handler1
 where
  handler1
    :: forall a
     . ((forall x . Either e (eff x) -> eff (Either e x))
        -> eff (Either e (eff (Either e a))))
    -> eff (Either e a)
  handler1 cont1 = do
    res <- cont1 contra1
    case res of
      (Left e) -> return $ Left e
      (Right mx) -> mx

  contra1 :: forall a . Either e (eff a) -> eff (Either e a)
  contra1 (Left e) = return $ Left e
  contra1 (Right mx) = mx >>= return . Right
