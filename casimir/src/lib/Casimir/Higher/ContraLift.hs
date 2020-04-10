
module Casimir.Higher.ContraLift
where

type ContraFree eff f =
  forall a
   . (forall w
       . (Functor w)
     => (forall x . f (eff x) -> eff (w x))
     -> eff (w (eff (f a))))
  -> eff (f a)
