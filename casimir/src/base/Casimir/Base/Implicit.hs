{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Casimir.Base.Implicit
  ( ImplicitOps
  , OpsConstraint
  , EffConstraint
  , Eff
  )
where

import QuasiParam.Casimir

import Casimir.Base.Effect
import Casimir.Base.List

class
  ( Effects eff
  , MultiParam (Operations eff)
  )
  => ImplicitOps eff

instance
  ( Effects eff
  , MultiParam (Operations eff)
  )
  => ImplicitOps eff

class
  ( ImplicitOps eff
  , ParamConstraint (Operations eff) m
  )
  => OpsConstraint eff m

instance
  ( ImplicitOps eff
  , ParamConstraint (Operations eff) m
  )
  => OpsConstraint eff m

class
  ( Monad m
  , EffectList effs
  , OpsConstraint (ToEffects effs) m
  )
  => EffConstraint effs m

instance
  ( Monad m
  , EffectList effs
  , OpsConstraint (ToEffects effs) m
  )
  => EffConstraint effs m

type Eff effs a = forall m . (EffConstraint effs m) => m a
