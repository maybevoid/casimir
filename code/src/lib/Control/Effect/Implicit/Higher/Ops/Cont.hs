
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

instance
  (Effect inEff)
  => EffFunctor (ContOps inEff) where
    effmap _ = undefined

instance HigherEffFunctor ContOps where
  invEffmap
    :: forall w eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => (forall x . eff1 x -> eff2 x)
    -> ContraLift w eff1 eff2
    -> ContOps eff1 eff1
    -> ContOps eff2 eff2
  invEffmap lifter (ContraLift contraLift1) ops =
    ContOps callCC2
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
    callCC2 cont1 = contraLift1 cont2
     where
      cont2
        :: (forall x . eff2 x -> eff1 (w x))
        -> eff1 (w a)
      cont2 contraLift2 = callCC1 cont3
       where
        cont3 :: (w a -> eff1 b) -> eff1 (w a)
        cont3 cont4 = contraLift2 cont5
         where
          cont5 :: eff2 a
          cont5 = cont1 cont6

          cont6 :: a -> eff2 b
          cont6 x1 = lifter $ do
            x2 :: w a <- contraLift2 $ return x1
            cont4 x2

  contraEffmap _ = undefined

contTOps
  :: forall eff r . (Effect eff)
  => ContOps (ContT.ContT r eff) (ContT.ContT r eff)
contTOps = ContOps ContT.callCC
