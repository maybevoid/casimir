
module Control.Effect.Implicit.Higher.ContraLift
where

import Control.Monad.Identity

import Control.Effect.Implicit.Base

newtype ContraLift eff1 eff2 = ContraLift {
  runContraLift
    :: forall a
     . (forall w
         . (Functor w)
        => (forall x . eff2 x -> eff1 (w x))
        -> eff1 (w a))
    -> eff2 a
}

type ContraFree eff f =
  forall a
   . (forall w
       . (Functor w)
     => (forall x . f (eff x) -> eff (w x))
     -> eff (w (eff (f a))))
  -> eff (f a)

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
