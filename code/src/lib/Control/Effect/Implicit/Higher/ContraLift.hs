
module Control.Effect.Implicit.Higher.ContraLift
where

data ContraLiftOps eff1 eff2 w = ContraLiftOps
  { contraLiftEff :: forall x . eff2 x -> eff1 (w x)
  , liftResume :: forall x . w x -> eff2 x
  }

newtype ContraLiftEff eff1 eff2 = ContraLiftEff {
  withContraLift
    :: forall r
     . (forall w . ContraLiftOps eff1 eff2 w -> eff2 r)
    -> eff2 r
}
