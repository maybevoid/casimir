
module Control.Effect.Implicit.Higher.Ops.Reader
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher
import Control.Effect.Implicit.Ops.Env

data ReaderEff e

data ReaderOps e inEff eff = ReaderOps
  { innerEnvOps :: EnvOps e inEff
  , outerEnvOps :: EnvOps e eff
  , localOp
      :: forall a
       . (e -> e)
      -> inEff a
      -> eff a
  }

instance HigherOps (ReaderEff e) where
  type HOperation (ReaderEff e) = ReaderOps e

instance HigherEffFunctor (ReaderOps e) where
  invEffmap
    :: forall eff1 eff2
     . ( Effect eff1
       , Effect eff2
       )
    => (forall x . eff1 x -> eff2 x)
    -> ContraLiftEff eff1 eff2
    -> ReaderOps e eff1 eff1
    -> ReaderOps e eff2 eff2
  invEffmap lifter contraLift1 ops
    = ReaderOps
        (effmap lifter $ innerEnvOps ops)
        (effmap lifter $ outerEnvOps ops)
        local
    where
      local :: forall a . (e -> e) -> eff2 a -> eff2 a
      local modifyEnv comp1 = withContraLift contraLift1 cont
       where
        cont :: forall w
              . ContraLiftOps eff1 eff2 w
             -> eff2 a
        cont contraLift2 = do
          res <- lifter comp2
          liftResume contraLift2 res
         where
          comp2 :: eff1 (w a)
          comp2 = localOp ops modifyEnv $
            contraLiftEff contraLift2 comp1
