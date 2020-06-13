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
  , OpsConstraint (List effs) m
  )
  => EffConstraint effs m

instance
  ( Monad m
  , OpsConstraint (List effs) m
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
   . ( EffConstraint '[eff] m
     )
  => Operation eff m
captureOp = ops
 where
  ops :+ _ = captureOps @(List '[eff])

withOp
  :: forall eff m r
   . ( Effect eff )
  => Operation eff m
  -> ((OpsConstraint (List '[eff]) m) => r)
  -> r
withOp ops = withOps $ ops :+ NoOp
