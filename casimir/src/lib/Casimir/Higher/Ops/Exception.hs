
module Casimir.Higher.Ops.Exception
where

import Data.Void

import qualified Control.Exception as Ex

import QuasiParam.Tag

import Casimir.Base
  ( Eff
  , Lift (..)
  , EffConstraint
  , ContraLift (..)
  , HigherLift (..)
  , EffFunctor (..)
  , ImplicitOps (..)
  , type (∪)
  , type (~>)
  )

import Casimir.Ops.Io
import qualified Casimir.Base as Base
import qualified Casimir.Higher as Higher

import Casimir.Higher
import Casimir.Higher.Free
import Casimir.Higher.ContraLift.Either

data ExceptionEff e

data ExceptionTag

data HigherExceptionOps e inEff eff = HigherExceptionOps
  { tryOp
      :: forall a
      . inEff a
      -> eff (Either e a)

  , throwOp :: e -> inEff Void
  }

type ExceptionOps e = LowerOps (HigherExceptionOps e)

pattern ExceptionOps
  :: forall e eff
   . (forall a. eff a -> eff (Either e a))
  -> (e -> eff Void)
  -> LowerOps (HigherExceptionOps e) eff
pattern ExceptionOps try throw = LowerOps (HigherExceptionOps try throw)

data ExceptionCoOp e f r where
  TryOp
    :: forall e f r
     . f r
    -> ExceptionCoOp e f (Either e r)

  ThrowOp
    :: forall e f r
     . e
    -> ExceptionCoOp e f r

instance Base.EffOps (ExceptionEff e) where
  type Operation (ExceptionEff e) = ExceptionOps e

instance Higher.EffOps (ExceptionEff e) where
  type Operation (ExceptionEff e) = HigherExceptionOps e

instance EffCoOp (ExceptionEff e) where
  type CoOperation (ExceptionEff e) = ExceptionCoOp e

instance LowerEffOps (ExceptionEff e)

instance ImplicitOps (ExceptionEff e) where
  type OpsConstraint (ExceptionEff e) eff =
    Param ExceptionTag (LowerOps (HigherExceptionOps e) eff)

  withOps = withParam @ExceptionTag
  captureOps = captureParam @ExceptionTag

instance
  (Effect inEff)
  => EffFunctor Lift (HigherExceptionOps e inEff) where
    effmap
      :: forall eff1 eff2
       . (Effect eff1, Effect eff2)
      => Lift eff1 eff2
      -> HigherExceptionOps e inEff eff1
      -> HigherExceptionOps e inEff eff2
    effmap (Lift lift) ops1 =
      HigherExceptionOps handleTry handleThrow
     where
      handleTry
        :: forall a
         . inEff a
        -> eff2 (Either e a)
      handleTry comp = lift $ tryOp ops1 comp

      handleThrow = throwOp ops1

instance HigherEffFunctor HigherLift (HigherExceptionOps e) where
  higherEffmap
    :: forall eff1 eff2
     . (Effect eff1, Effect eff2)
    => HigherLift eff1 eff2
    -> HigherExceptionOps e eff1 eff1
    -> HigherExceptionOps e eff2 eff2
  higherEffmap
    (HigherLift lifter contraLift1)
    (HigherExceptionOps handleTry1 handleThrow1) =
      HigherExceptionOps handleTry2 handleThrow2
   where
    handleTry2
      :: forall a
       . eff2 a
      -> eff2 (Either e a)
    handleTry2 comp1 = runContraLift contraLift1 cont1
     where
      cont1
        :: forall w
         . (Functor w)
        => (forall x . eff2 x -> eff1 (w x))
        -> eff1 (w (Either e a))
      cont1 contraLift2 = do
        res1 <- comp3
        case res1 of
          Left (e :: e) ->
            contraLift2 $ return $ Left e
          Right (wa :: w a) ->
            return $ fmap Right wa

       where
        comp2 :: eff1 (w a)
        comp2 = contraLift2 comp1

        comp3 :: eff1 (Either e (w a))
        comp3 = handleTry1 comp2

    handleThrow2 :: e -> eff2 Void
    handleThrow2 = lifter . handleThrow1

instance CoOpFunctor (ExceptionCoOp e) where
  liftCoOp
    :: forall f1 f2 a
     . (Functor f1, Functor f2)
    => f1 ~> f2
    -> ExceptionCoOp e f1 a
    -> ExceptionCoOp e f2 a
  liftCoOp lifter (TryOp comp) =
    TryOp (lifter comp)

  liftCoOp _ (ThrowOp e) = ThrowOp e

instance FreeOps (ExceptionEff e) where
  mkFreeOps
    :: forall eff
    . (Effect eff)
    => (forall a . ExceptionCoOp e eff a -> eff a)
    -> HigherExceptionOps e eff eff
  mkFreeOps lifter = HigherExceptionOps handleTry handleThrow
   where
    handleTry
      :: forall a
       . eff a
      -> eff (Either e a)
    handleTry comp = lifter $ TryOp comp

    handleThrow e = lifter $ ThrowOp e

exceptionCoOpHandler
  :: forall e eff
   . (Effect eff)
  => CoOpHandler (ExceptionEff e) (Either e) eff
exceptionCoOpHandler = CoOpHandler
  (return . Right) handleOp contraEither
 where
  handleOp
    :: forall a r
     . ExceptionCoOp e (eff ∘ Either e) a
    -> (a -> eff (Either e r))
    -> eff (Either e r)
  handleOp (TryOp (Nest comp)) cont = do
    res1 <- comp
    cont res1
  handleOp (ThrowOp e) _ = return $ Left e

tryIo
  :: forall eff e1 e2 a
   . ( Ex.Exception e1
     , EffConstraint (ExceptionEff e2 ∪ IoEff) eff
     )
  => (e1 -> e2)
  -> IO a
  -> Eff (IoEff ∪ (ExceptionEff e2)) a
tryIo liftErr comp = do
  res <- liftIo $ Ex.try comp
  case res of
    Left err -> throw $ liftErr err
    Right val -> return val

tryIoOps
  :: forall eff e1 e2
   . ( Ex.Exception e1
     , EffConstraint (ExceptionEff e2 ∪ IoEff) eff
     )
  => (e1 -> e2)
  -> IoOps eff
tryIoOps liftErr = IoOps $ tryIo liftErr

try
  :: forall eff e a
   . (EffConstraint (ExceptionEff e) eff)
  => eff a
  -> eff (Either e a)
try = tryOp $ unLowerOps captureOps

throw
  :: forall eff e a
   . (EffConstraint (ExceptionEff e) eff)
  => e
  -> eff a
throw e = throwOp (unLowerOps captureOps) e >>= absurd

tryCatch
  :: forall eff e a
   . (EffConstraint (ExceptionEff e) eff)
  => eff a
  -> (e -> eff a)
  -> eff a
tryCatch comp handler = do
  res <- try comp
  case res of
    Left e ->
      handler e
    Right x ->
      return x

tryFinally
  :: forall eff e a
   . (EffConstraint (ExceptionEff e) eff)
  => eff a
  -> eff ()
  -> eff a
tryFinally comp finalizer = do
  res <- try comp
  finalizer
  case res of
    Left e ->
      throw e
    Right x ->
      return x

tryCatchFinally
  :: forall eff e a
   . (EffConstraint (ExceptionEff e) eff)
  => eff a
  -> (e -> eff a)
  -> eff ()
  -> eff a
tryCatchFinally comp handler finalizer = do
  res <- try comp
  finalizer
  case res of
    Left e ->
      handler e
    Right x ->
      return x
