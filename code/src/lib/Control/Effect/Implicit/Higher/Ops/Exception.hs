
module Control.Effect.Implicit.Higher.Ops.Exception
where

import Data.Void

import Control.Effect.Implicit.Higher
import Control.Effect.Implicit.Higher.Free
import Control.Effect.Implicit.Higher.ContraLift.Either

import qualified Control.Effect.Implicit.Base as Base

data ExceptionEff e

data ExceptionOps e inEff eff = ExceptionOps
  { tryOp
      :: forall a
      . inEff a
      -> (e -> inEff a)
      -> eff a

  , throwOp :: e -> inEff Void
  }

data ExceptionCoOp e f r where
  TryOp
    :: forall e f r
     . f r
    -> (e -> f r)
    -> ExceptionCoOp e f r

  ThrowOp
    :: forall e f r
     . e
    -> ExceptionCoOp e f r

instance EffOps (ExceptionEff e) where
  type Operation (ExceptionEff e) = ExceptionOps e

instance EffCoOp (ExceptionEff e) where
  type CoOperation (ExceptionEff e) = ExceptionCoOp e

instance
  (Effect inEff)
  => Base.EffFunctor (ExceptionOps e inEff) where
    effmap
      :: forall eff1 eff2
       . (Effect eff1, Effect eff2)
      => (forall x . eff1 x -> eff2 x)
      -> ExceptionOps e inEff eff1
      -> ExceptionOps e inEff eff2
    effmap lifter ops1 = ExceptionOps handleTry handleThrow
     where
      handleTry
        :: forall a
         . inEff a
        -> (e -> inEff a)
        -> eff2 a
      handleTry comp handler = lifter $ tryOp ops1 comp handler
      handleThrow = throwOp ops1

instance EffFunctor (ExceptionOps e) where
  invEffmap
    :: forall eff1 eff2
     . (Effect eff1, Effect eff2)
    => (forall x . eff1 x -> eff2 x)
    -> ContraLift eff1 eff2
    -> ExceptionOps e eff1 eff1
    -> ExceptionOps e eff2 eff2
  invEffmap lifter contraLift1 (ExceptionOps handleTry1 handleThrow1) =
    ExceptionOps handleTry2 handleThrow2
   where
    handleTry2
      :: forall a
       . eff2 a
      -> (e -> eff2 a)
      -> eff2 a
    handleTry2 comp1 handler1 = runContraLift contraLift1 cont1
     where
      cont1
        :: forall w
         . (Functor w)
        => (forall x . eff2 x -> eff1 (w x))
        -> eff1 (w a)
      cont1 contraLift2 = handleTry1 comp2 handler2
       where
        comp2 :: eff1 (w a)
        comp2 = contraLift2 comp1

        handler2 :: e -> eff1 (w a)
        handler2 e = contraLift2 $ handler1 e

    handleThrow2 :: e -> eff2 Void
    handleThrow2 = lifter . handleThrow1

instance
  (Functor f)
  => Functor (ExceptionCoOp e f) where
    fmap
      :: forall a b
       . (a -> b)
      -> ExceptionCoOp e f a
      -> ExceptionCoOp e f b
    fmap f (TryOp comp handler) =
      TryOp (fmap f comp) (fmap (fmap f) handler)
    fmap _ (ThrowOp e) = ThrowOp e

instance CoOpFunctor (ExceptionEff e) where
  liftCoOp
    :: forall f1 f2 a
     . (Functor f1, Functor f2)
    => (forall x . f1 x -> f2 x)
    -> ExceptionCoOp e f1 a
    -> ExceptionCoOp e f2 a
  liftCoOp lifter (TryOp comp handler) =
    TryOp (lifter comp) (fmap lifter handler)

  liftCoOp _ (ThrowOp e) = ThrowOp e

  mapCoOp = fmap

instance FreeOps (ExceptionEff e) where
  mkFreeOps
    :: forall eff
    . (Effect eff)
    => (forall a . ExceptionCoOp e eff a -> eff a)
    -> ExceptionOps e eff eff
  mkFreeOps lifter = ExceptionOps handleTry handleThrow
   where
    handleTry
      :: forall a
       . eff a
      -> (e -> eff a)
      -> eff a
    handleTry comp handler = lifter $ TryOp comp handler

    handleThrow e = lifter $ ThrowOp e

tryComp
  :: forall eff e a
   . (Effect eff)
  => (ExceptionOps e eff eff)
  -> eff a
  -> eff (Either e a)
tryComp (ExceptionOps try _) comp =
  try (comp >>= return . Right) (return . Left)

exceptionCoOpHandler
  :: forall e eff
   . (Effect eff)
  => CoOpHandler (ExceptionEff e) eff (Either e)
exceptionCoOpHandler = CoOpHandler
  (return . Right) handleOp contraEither
 where
  handleOp
    :: forall a r
     . ExceptionCoOp e (eff ∘ Either e) a
    -> (a -> eff (Either e r))
    -> eff (Either e r)
  handleOp (TryOp (Nest comp) handleErr) cont = do
    res1 <- comp
    case res1 of
      Left e1 -> do
        res2 <- unNest $ handleErr e1
        case res2 of
          Left e2 -> return $ Left e2
          Right a -> cont a
      Right a -> cont a
  handleOp (ThrowOp e) _ = return $ Left e
