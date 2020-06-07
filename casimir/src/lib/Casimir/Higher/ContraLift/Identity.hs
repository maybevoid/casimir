
module Casimir.Higher.ContraLift.Identity
where

import Control.Monad.Identity

import Casimir.Higher.ContraLift

contraIdentity
  :: forall m
   . (Monad m)
  => ContraFree m Identity
contraIdentity = handler1
 where
  handler1
    :: forall a
     . ((forall x . Identity (m x) -> m (Identity x))
        -> m (Identity (m (Identity a))))
    -> m (Identity a)
  handler1 cont1 = cont1 contra1 >>= runIdentity

  contra1 :: forall a . Identity (m a) -> m (Identity a)
  contra1 (Identity mx) = mx >>= return . Identity
