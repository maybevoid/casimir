
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

data ReaderOps e inEff eff = ReaderOps
  { innerEnvOps :: EnvOps e inEff
  , outerEnvOps :: EnvOps e eff
  , localOp
      :: forall a
       . (e -> e)
      -> inEff a
      -> eff a
  }

instance EffOps (ReaderEff e) where
  type Operation (ReaderEff e) = ReaderOps e

instance
  (Monad inEff)
  => EffFunctor Lift (ReaderOps e inEff) where
    effmap _ = undefined

instance HigherEffFunctor HigherLift (ReaderOps e) where
  higherEffmap
    :: forall eff1 eff2
     . ( Monad eff1
       , Monad eff2
       )
    => HigherLift eff1 eff2
    -> ReaderOps e eff1 eff1
    -> ReaderOps e eff2 eff2
  higherEffmap
    (HigherLift lift (ContraLift contraLift1)) ops
    = ReaderOps
        (effmap (Lift lift) $ innerEnvOps ops)
        (effmap (Lift lift) $ outerEnvOps ops)
        local
    where
      local :: forall a . (e -> e) -> eff2 a -> eff2 a
      local modifyEnv comp1 = contraLift1 cont
       where
        cont
          :: forall w
           . (forall x . eff2 x -> eff1 (w x))
          -> eff1 (w a)
        cont contraLift2 =
          localOp ops modifyEnv $
            contraLift2 comp1
