
module Control.Effect.Implicit.Higher.ContraLift
where

import Control.Effect.Implicit.Base

data ContraLiftOps eff1 eff2 w = ContraLiftOps
  { contraLiftEff :: forall x . eff2 x -> eff1 (w x)
  , liftResume :: forall x . w x -> eff2 x
  }

data ContraLiftEff eff1 eff2 where
  ContraLiftEff
    :: forall eff1 eff2 w
     . eff2 (ContraLiftOps eff1 eff2 w)
    -> ContraLiftEff eff1 eff2

type ContraLiftOps' eff1 eff2 w =
  forall a
   . ((forall x . eff2 x -> eff1 (w x))
      -> eff1 (w a))
  -> eff2 a

data ContraLiftEff' eff1 eff2 where
  ContraLiftEff'
    :: forall eff1 eff2 w
     . ContraLiftOps' eff1 eff2 w
    -> ContraLiftEff' eff1 eff2

withContraLift
  :: forall eff1 eff2 r
   . (Effect eff1, Effect eff2)
  => ContraLiftEff eff1 eff2
  -> (forall w . ContraLiftOps eff1 eff2 w -> eff2 r)
  -> eff2 r
withContraLift (ContraLiftEff ops1) cont = do
  ops2 <- ops1
  cont ops2

withContraLift'
  :: forall eff1 eff2 r
   . (Effect eff1, Effect eff2)
  => ContraLiftEff eff1 eff2
  -> (forall w . eff2 (ContraLiftOps eff1 eff2 w) -> eff2 r)
  -> eff2 r
withContraLift' (ContraLiftEff ops) cont = cont ops
