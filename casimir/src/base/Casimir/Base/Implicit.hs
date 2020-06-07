{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Casimir.Base.Implicit
  ( ImplicitOps
  , EffConstraint
  , Eff
  )
where

import QuasiParam.Casimir

import Casimir.Base.Effect

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

type EffConstraint eff m = (Monad m, OpsConstraint eff m)

type Eff eff a = forall m . (EffConstraint eff m) => m a
