
module Casimir.Higher.ContraLift
where

type ContraFree m f =
  forall a
   . (forall w
       . (Functor w)
     => (forall x . f (m x) -> m (w x))
     -> m (w (m (f a))))
  -> m (f a)
