
module Control.Effect.Implicit.Higher.Ops.Reader
where

import Control.Effect.Implicit.Higher
import Control.Effect.Implicit.Ops.Env
import qualified Control.Effect.Implicit.Base as Base

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
  (Effect inEff)
  => EffFunctor (ReaderOps e inEff) where
    effmap _ = undefined

instance HigherEffFunctor (ReaderOps e) where
  invEffmap
    :: forall eff1 eff2
     . ( Effect eff1
       , Effect eff2
       )
    => (forall x . eff1 x -> eff2 x)
    -> ContraLift eff1 eff2
    -> ReaderOps e eff1 eff1
    -> ReaderOps e eff2 eff2
  invEffmap lifter (ContraLift contraLift1) ops
    = ReaderOps
        (Base.effmap lifter $ innerEnvOps ops)
        (Base.effmap lifter $ outerEnvOps ops)
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
