{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Casimir.Base.Implicit
  ( ImplicitOps
  , OpsConstraint
  , EffConstraint
  , Eff
  , withOp
  , withOps
  , captureOp
  , captureOps
  )
where

import QuasiParam.Casimir (MultiParam (..))
import qualified QuasiParam.Casimir as Param

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

instance
  ( ImplicitOps eff
  , ParamConstraint (Operations eff) m
  )
  => OpsConstraint eff m

class
  ( Monad m
  , OpsConstraint effs m
  )
  => EffConstraint effs m

instance
  ( Monad m
  , Effects effs
  , OpsConstraint effs m
  )
  => EffConstraint effs m

type Eff effs a = forall m . (EffConstraint effs m) => m a

captureOps
  :: forall eff m
   . ( OpsConstraint eff m )
  => Operations eff m
captureOps = captureParam

withOps
  :: forall eff m r
   . ( ImplicitOps eff )
  => Operations eff m
  -> ((OpsConstraint eff m) => r)
  -> r
withOps = withParam

captureOp
  :: forall eff m
   . ( Effect eff
     , OpsConstraint (Singleton eff) m
     )
  => Operation eff m
captureOp = Param.unSingleton captureOps

withOp
  :: forall eff m r
   . ( Effect eff
     , ImplicitOps (Singleton eff)
     )
  => Operation eff m
  -> ((OpsConstraint (Singleton eff) m) => r)
  -> r
withOp ops = withOps (Param.Singleton ops)
