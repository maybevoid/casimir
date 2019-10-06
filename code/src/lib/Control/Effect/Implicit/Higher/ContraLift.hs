
module Control.Effect.Implicit.Higher.ContraLift
where

newtype ContraLift w eff1 eff2 = ContraLift {
  doContraLift
    :: forall a
     . ((forall x . eff2 x -> eff1 (w x))
        -> eff1 (w a))
    -> eff2 a
}
