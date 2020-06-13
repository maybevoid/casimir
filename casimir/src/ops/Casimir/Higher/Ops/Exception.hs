
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
  , HasLabel (..)
  , Tag
  , type (∪)
  , type (~>)
  , captureOp
  )

import Casimir.Ops.Io
import qualified Casimir.Base as Base
import qualified Casimir.Higher as Higher

import Casimir.Higher
import Casimir.Higher.Free
import Casimir.Higher.ContraLift.Either

data ExceptionTag
data ExceptionEff e

data HigherExceptionOps e inEff m = HigherExceptionOps
  { tryOp
      :: forall a
      . inEff a
      -> m (Either e a)

  , throwOp :: e -> inEff Void
  }

type ExceptionOps e = LowerOps (HigherExceptionOps e)

pattern ExceptionOps
  :: forall e m
   . (forall a. m a -> m (Either e a))
  -> (e -> m Void)
  -> LowerOps (HigherExceptionOps e) m
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

instance Base.Effect (ExceptionEff e) where
  type Operation (ExceptionEff e) = ExceptionOps e

instance Higher.Effect (ExceptionEff e) where
  type Operation (ExceptionEff e) = HigherExceptionOps e

instance HasLabel (HigherExceptionOps e) where
  type GetLabel (HigherExceptionOps e) = Tag ExceptionTag

instance
  (Monad inEff)
  => EffFunctor Lift (HigherExceptionOps e inEff) where
    effmap
      :: forall m1 m2
       . (Monad m1, Monad m2)
      => Lift m1 m2
      -> HigherExceptionOps e inEff m1
      -> HigherExceptionOps e inEff m2
    effmap (Lift lift) ops1 =
      HigherExceptionOps handleTry handleThrow
     where
      handleTry
        :: forall a
         . inEff a
        -> m2 (Either e a)
      handleTry comp = lift $ tryOp ops1 comp

      handleThrow = throwOp ops1

instance HigherEffFunctor HigherLift (HigherExceptionOps e) where
  higherEffmap
    :: forall m1 m2
     . (Monad m1, Monad m2)
    => HigherLift m1 m2
    -> HigherExceptionOps e m1 m1
    -> HigherExceptionOps e m2 m2
  higherEffmap
    (HigherLift lifter contraLift1)
    (HigherExceptionOps handleTry1 handleThrow1) =
      HigherExceptionOps handleTry2 handleThrow2
   where
    handleTry2
      :: forall a
       . m2 a
      -> m2 (Either e a)
    handleTry2 comp1 = runContraLift contraLift1 cont1
     where
      cont1
        :: forall w
         . (Functor w)
        => (forall x . m2 x -> m1 (w x))
        -> m1 (w (Either e a))
      cont1 contraLift2 = do
        res1 <- comp3
        case res1 of
          Left (e :: e) ->
            contraLift2 $ return $ Left e
          Right (wa :: w a) ->
            return $ fmap Right wa

       where
        comp2 :: m1 (w a)
        comp2 = contraLift2 comp1

        comp3 :: m1 (Either e (w a))
        comp3 = handleTry1 comp2

    handleThrow2 :: e -> m2 Void
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

instance FreeOps (HigherExceptionOps e) where
  type CoOperation (HigherExceptionOps e) = ExceptionCoOp e

  mkFreeOps
    :: forall m
    . (Monad m)
    => (forall a . ExceptionCoOp e m a -> m a)
    -> HigherExceptionOps e m m
  mkFreeOps lifter = HigherExceptionOps handleTry handleThrow
   where
    handleTry
      :: forall a
       . m a
      -> m (Either e a)
    handleTry comp = lifter $ TryOp comp

    handleThrow e = lifter $ ThrowOp e

exceptionCoOpHandler
  :: forall e m
   . (Monad m)
  => CoOpHandler (HigherExceptionOps e) (Either e) m
exceptionCoOpHandler = CoOpHandler
  (return . Right) handleOp contraEither
 where
  handleOp
    :: forall a r
     . ExceptionCoOp e (m ∘ Either e) a
    -> (a -> m (Either e r))
    -> m (Either e r)
  handleOp (TryOp (Nest comp)) cont = do
    res1 <- comp
    cont res1
  handleOp (ThrowOp e) _ = return $ Left e

tryIo
  :: forall m e1 e2 a
   . ( Ex.Exception e1
     )
  => (e1 -> e2)
  -> IO a
  -> Eff '[IoEff, (ExceptionEff e2)] a
tryIo liftErr comp = do
  res <- liftIo $ Ex.try comp
  case res of
    Left err -> throw $ liftErr err
    Right val -> return val

tryIoOps
  :: forall m e1 e2
   . ( Ex.Exception e1
     , EffConstraint '[ExceptionEff e2, IoEff] m
     )
  => (e1 -> e2)
  -> IoOps m
tryIoOps liftErr = IoOps $ tryIo liftErr

try
  :: forall m e a
   . (EffConstraint '[ExceptionEff e] m)
  => m a
  -> m (Either e a)
try = tryOp $ unLowerOps captureOp

throw
  :: forall e a
   . e
  -> Eff '[ExceptionEff e] a
throw e = throwOp (unLowerOps captureOp) e >>= absurd

tryCatch
  :: forall m e a
   . (EffConstraint '[ExceptionEff e] m)
  => m a
  -> (e -> m a)
  -> m a
tryCatch comp handler = do
  res <- try comp
  case res of
    Left e ->
      handler e
    Right x ->
      return x

tryFinally
  :: forall m e a
   . (EffConstraint '[ExceptionEff e] m)
  => m a
  -> m ()
  -> m a
tryFinally comp finalizer = do
  res <- try comp
  finalizer
  case res of
    Left e ->
      throw e
    Right x ->
      return x

tryCatchFinally
  :: forall m e a
   . (EffConstraint '[ExceptionEff e] m)
  => m a
  -> (e -> m a)
  -> m ()
  -> m a
tryCatchFinally comp handler finalizer = do
  res <- try comp
  finalizer
  case res of
    Left e ->
      handler e
    Right x ->
      return x
