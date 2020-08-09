
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

data ContOps m1 m2 = ContOps {
  callCCOp
    :: forall a b
     . ((a -> m1 b) -> m1 a)
    -> m2 a
}

instance
  (Monad m1)
  => EffFunctor Lift (ContOps m1) where
    effmap
      :: forall m2 m3
       . Lift m2 m3
      -> ContOps m1 m2
      -> ContOps m1 m3
    effmap (Lift lift) (ContOps callCC) = ContOps $ \cont ->
      lift $ callCC cont

instance EffFunctor HigherLift (LowerOps (Ops ContOps)) where
  effmap
    :: forall m1 m2
      . ( Monad m1
        , Monad m2
        )
    => HigherLift m1 m2
    -> LowerOps (Ops ContOps) m1
    -> LowerOps (Ops ContOps) m2
  effmap
    (HigherLift lift (ContraLift contraLift1))
    (LowerOps (Ops ops)) =
      LowerOps $ Ops $ ContOps callCC2
   where
    callCC1
      :: forall a b
      . ((a -> m1 b) -> m1 a)
      -> m1 a
    callCC1 = callCCOp ops

    callCC2
      :: forall a b
       . ((a -> m2 b) -> m2 a)
      -> m2 a
    callCC2 cont1 = contraLift1 cont2
     where
      cont2
        :: forall w
         . (forall x . m2 x -> m1 (w x))
        -> m1 (w a)
      cont2 contraLift2 = callCC1 cont3
       where
        cont3 :: (w a -> m1 b) -> m1 (w a)
        cont3 cont4 = contraLift2 cont5
         where
          cont5 :: m2 a
          cont5 = cont1 cont6

          cont6 :: a -> m2 b
          cont6 x1 = lift $ do
            x2 :: w a <- contraLift2 $ return x1
            cont4 x2

contTOps
  :: forall m r . (Monad m)
  => ContOps (ContT.ContT r m) (ContT.ContT r m)
contTOps = ContOps ContT.callCC
