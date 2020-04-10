
module Casimir.Higher.ContraLift.Identity
where

import Control.Monad.Identity

import Casimir.Base
import Casimir.Higher.ContraLift

contraIdentity
  :: forall eff
   . (Effect eff)
  => ContraFree eff Identity
contraIdentity = handler1
 where
  handler1
    :: forall a
     . ((forall x . Identity (eff x) -> eff (Identity x))
        -> eff (Identity (eff (Identity a))))
    -> eff (Identity a)
  handler1 cont1 = cont1 contra1 >>= runIdentity

  contra1 :: forall a . Identity (eff a) -> eff (Identity a)
  contra1 (Identity mx) = mx >>= return . Identity
