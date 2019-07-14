
module Control.Effect.Implicit.Higher.ContraLift
where

data ContraLiftEff' eff1 eff2 w = ContraLiftEff'
  { contraLiftEff :: forall x . eff2 x -> eff1 (w x)
  , liftResume :: forall x . w x -> eff2 x
  }

data ContraLiftEff eff1 eff2 where
  ContraLiftEff
    :: forall eff1 eff2 w
     . eff2 (ContraLiftEff' eff1 eff2 w)
    -> ContraLiftEff eff1 eff2
