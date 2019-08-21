
module Control.Effect.Implicit.Higher.Ops.Cont
where

import qualified Control.Monad.Trans.Cont as ContT

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher

data ContOps inEff eff = ContOps {
  callCCOp
    :: forall a b
     . ((a -> inEff b) -> inEff a)
    -> eff a
}

instance HigherEffFunctor ContOps where
  invEffmap
    :: forall eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => (forall x . eff1 x -> eff2 x)
    -> ContraLiftEff eff1 eff2
    -> ContOps eff1 eff1
    -> ContOps eff2 eff2
  invEffmap lifter contraLift1 ops = ContOps callCC2
   where
    callCC1
      :: forall a b
      . ((a -> eff1 b) -> eff1 a)
      -> eff1 a
    callCC1 = callCCOp ops

    callCC2
      :: forall a b
       . ((a -> eff2 b) -> eff2 a)
      -> eff2 a
    callCC2 cont1 = withContraLift' contraLift1 cont2
     where
      cont2 :: forall w
             . eff2 (ContraLiftOps eff1 eff2 w)
            -> eff2 a
      cont2 contraLift2 = do
        contraLift3 <- contraLift2
        res <- lifter $ cont3 contraLift3
        liftResume contraLift3 res
       where
        cont3
          :: ContraLiftOps eff1 eff2 w
          -> eff1 (w a)
        cont3 contraLift3 = callCC1 cont4
         where
          cont4 :: (w a -> eff1 b) -> eff1 (w a)
          cont4 cont5 = contraLiftEff contraLift3 cont9
           where
            cont6 :: a -> eff2 b
            cont6 x = do
              contraLift4 <- contraLift2
              lifter $ cont8 $ cont7 contraLift4
             where
              cont7 :: ContraLiftOps eff1 eff2 w -> eff1 (w a)
              cont7 contraLift4 = contraLiftEff contraLift4 $ return x

              cont8 :: eff1 (w a) -> eff1 b
              cont8 cont7' = do
                mx <- cont7'
                cont5 mx

            cont9 :: eff2 a
            cont9 = cont1 cont6

contTOps
  :: forall eff r . (Effect eff)
  => ContOps (ContT.ContT r eff) (ContT.ContT r eff)
contTOps = ContOps ContT.callCC
