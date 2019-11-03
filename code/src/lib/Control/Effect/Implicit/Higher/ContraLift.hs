
module Control.Effect.Implicit.Higher.ContraLift
where

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
