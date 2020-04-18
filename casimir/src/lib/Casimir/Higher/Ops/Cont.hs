
module Casimir.Higher.Ops.Cont
where

import qualified Control.Monad.Trans.Cont as ContT

import Casimir.Base
  ( ContraLift (..)
  , EffFunctor (..)
  , Lift (..)
  , HigherLift (..)
  , ContraLift (..)
  )
import Casimir.Higher

data ContOps eff1 eff2 = ContOps {
  callCCOp
    :: forall a b
     . ((a -> eff1 b) -> eff1 a)
    -> eff2 a
}

instance
  (Effect eff1)
  => EffFunctor Lift (ContOps eff1) where
    effmap
      :: forall eff2 eff3
       . Lift eff2 eff3
      -> ContOps eff1 eff2
      -> ContOps eff1 eff3
    effmap (Lift lift) (ContOps callCC) = ContOps $ \cont ->
      lift $ callCC cont

instance HigherEffFunctor HigherLift ContOps where
  higherEffmap
    :: forall eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => HigherLift eff1 eff2
    -> ContOps eff1 eff1
    -> ContOps eff2 eff2
  higherEffmap
    (HigherLift lift (ContraLift contraLift1))
    ops =
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
        :: forall w
         . (forall x . eff2 x -> eff1 (w x))
        -> eff1 (w a)
      cont2 contraLift2 = callCC1 cont3
       where
        cont3 :: (w a -> eff1 b) -> eff1 (w a)
        cont3 cont4 = contraLift2 cont5
         where
          cont5 :: eff2 a
          cont5 = cont1 cont6

          cont6 :: a -> eff2 b
          cont6 x1 = lift $ do
            x2 :: w a <- contraLift2 $ return x1
            cont4 x2

contTOps
  :: forall eff r . (Effect eff)
  => ContOps (ContT.ContT r eff) (ContT.ContT r eff)
contTOps = ContOps ContT.callCC
