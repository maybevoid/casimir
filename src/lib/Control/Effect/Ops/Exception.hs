module Control.Effect.Ops.Exception
where

import Data.Void

import Control.Effect.Base

data ExceptionEff e where

data ExceptionOps e eff = ExceptionOps {
  raiseOp :: e -> eff Void
}

data ExceptionCoOp e r =
  RaiseOp e

type ExceptionConstraint e eff = (?exceptionOps :: ExceptionOps e eff)

instance EffFunctor (ExceptionOps e) where
  effmap lifter ops = ExceptionOps {
    raiseOp = \e -> lifter $ raiseOp ops e
  }

instance Functor (ExceptionCoOp e) where
  fmap _ (RaiseOp e) = RaiseOp e

instance FreeOps (ExceptionEff e) where
  type Operation (ExceptionEff e) = ExceptionOps e
  type CoOperation (ExceptionEff e) = ExceptionCoOp e

  mkFreeOps liftCoOp = ExceptionOps {
    raiseOp = \e -> liftCoOp $ RaiseOp e
  }

instance EffOps (ExceptionEff e) where
  type OpsConstraint (ExceptionEff e) eff = ExceptionConstraint e eff

  withOps ops comp = let ?exceptionOps = ops in comp

  captureOps = ?exceptionOps

raise
  :: forall e a eff
   . (Effect eff, ExceptionConstraint e eff)
  => e
  -> eff a
raise e = raiseOp captureOps e >>= absurd

mkExceptionCoOpHandler
  :: forall eff e a
   . (Effect eff)
  => (e -> eff a)
  -> CoOpHandler (ExceptionEff e) a a eff
mkExceptionCoOpHandler handleException = CoOpHandler {
  handleReturn = return,
  handleCoOp = \(RaiseOp e) -> handleException e
}

exceptionToEitherHandler
  :: forall eff e a
   . (Effect eff)
  => CoOpHandler (ExceptionEff e) a (Either e a) eff
exceptionToEitherHandler = CoOpHandler {
  handleReturn = \x -> return $ Right x,
  handleCoOp = \(RaiseOp e) -> return $ Left e
}