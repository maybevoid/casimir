{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Casimir.Base.Implicit
  ( OpsConstraint
  , EffConstraint
  , Eff
  , withOp
  , withOps
  , captureOp
  , captureOps
  )
where

import Casimir.Param (MultiParam (..))

import Casimir.Base.Effect

class
  ( Effects eff
  , ParamConstraint (Operations eff) m
  )
  => OpsConstraint eff m

instance
  ( Effects eff
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
   . ( Effects eff )
  => Operations eff m
  -> ((OpsConstraint eff m) => r)
  -> r
withOps = withParam

captureOp
  :: forall eff m
   . ( Effect eff
     , OpsConstraint '[eff] m
     )
  => Operation eff m
captureOp = ops
 where
  ops :+ _ = captureOps @'[eff]

withOp
  :: forall eff m r
   . ( Effect eff, Effects '[eff] )
  => Operation eff m
  -> ((OpsConstraint '[eff] m) => r)
  -> r
withOp ops = withOps $ ops :+ NoOp
