
module Casimir.Ops.Env.Higher
where

import Casimir.Base
  ( ContraLift (..)
  , EffFunctor (..)
  , HigherLift (..)
  , ContraLift (..)
  , Lift (..)
  )

import Casimir.Higher
import Casimir.Ops.Env.Base

data ReaderEff e

data ReaderOps e inEff m = ReaderOps
  { innerEnvOps :: EnvOps e inEff
  , outerEnvOps :: EnvOps e m
  , localOp
      :: forall a
       . (e -> e)
      -> inEff a
      -> m a
  }

instance EffOps (ReaderEff e) where
  type Operation (ReaderEff e) = ReaderOps e

instance
  (Monad inEff)
  => EffFunctor Lift (ReaderOps e inEff) where
    effmap _ = undefined

instance HigherEffFunctor HigherLift (ReaderOps e) where
  higherEffmap
    :: forall m1 m2
     . ( Monad m1
       , Monad m2
       )
    => HigherLift m1 m2
    -> ReaderOps e m1 m1
    -> ReaderOps e m2 m2
  higherEffmap
    (HigherLift lift (ContraLift contraLift1)) ops
    = ReaderOps
        (effmap (Lift lift) $ innerEnvOps ops)
        (effmap (Lift lift) $ outerEnvOps ops)
        local
    where
      local :: forall a . (e -> e) -> m2 a -> m2 a
      local modifyEnv comp1 = contraLift1 cont
       where
        cont
          :: forall w
           . (forall x . m2 x -> m1 (w x))
          -> m1 (w a)
        cont contraLift2 =
          localOp ops modifyEnv $
            contraLift2 comp1
