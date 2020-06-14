{-# language PolyKinds #-}

module Casimir.Computation.Value
  ( FunctorComp (..)
  , Return (..)
  , Arrow (..)
  , ReturnCtx (..)
  , genericComputation
  , genericReturn
  , runIdentityComp
  , execComp
  , arrowComputation
  )
where

import Data.Kind
import Control.Monad.Identity (Identity (..))

import Casimir.Base
import Casimir.Computation.Computation

class FunctorComp ret where
  mapComp
    :: forall a b m
     . (Monad m)
    => (a -> b)
    -> ret a m
    -> ret b m

newtype Return a m = Return {
  returnVal :: m a
}

newtype Arrow a ret b (m :: Type -> Type) = Arrow {
  arrowVal :: a -> ret b m
}

newtype ReturnCtx f ret a (m :: Type -> Type) = ReturnCtx {
  returnCtx :: ret (f a) m
}

instance EffFunctor Lift (Return a) where
  effmap (Lift lift) (Return mx) = Return $ lift mx

instance (EffFunctor lift (ret b)) => EffFunctor lift (Arrow a ret b) where
  effmap lifter (Arrow fn) = Arrow $
    \x -> effmap lifter $ fn x

instance
  (EffFunctor lift (ret (f a)))
  => EffFunctor lift (ReturnCtx f ret a)
   where
    effmap lifter (ReturnCtx mx) = ReturnCtx $ effmap lifter mx

instance FunctorComp Return where
  mapComp f (Return mx) = Return $
   do
    x <- mx
    return $ f x

instance (FunctorComp ret) => FunctorComp (Arrow a ret) where
  mapComp f (Arrow fn) = Arrow $
    mapComp f . fn

instance
  (Functor f, FunctorComp ret) =>
  FunctorComp (ReturnCtx f ret)
   where
    mapComp f (ReturnCtx mx) = ReturnCtx $
      mapComp (fmap f) mx

{-# INLINE genericComputation #-}
genericComputation
  :: forall ops comp lift
   . (Effects ops)
  => (forall m
       . (Monad m, OpsConstraint ops m)
       => comp m)
  -> (forall m . (Monad m) => Computation lift ops comp m)
genericComputation comp = Computation $
  \ _ ops -> withOps ops comp

{-# INLINE genericReturn #-}
genericReturn
  :: forall ops lift a
   . (Effects ops)
  => (forall m
       . (Monad m, OpsConstraint ops m)
       => m a)
  -> (forall m . (Monad m)
      => Computation lift ops (Return a) m)
genericReturn comp = genericComputation $ Return comp

arrowComputation
  :: forall ops lift a b
   . (Effects ops)
  => (forall m
        . (Monad m, OpsConstraint ops m)
       => a
       -> m b)
  -> (forall m . (Monad m)
       => Computation lift ops (Arrow a Return b) m)
arrowComputation fn = genericComputation $ Arrow $
  \x -> Return $ fn x

runIdentityComp
  :: forall a lift
   . (LiftMonoid lift)
  => Computation lift Nil (Return a) Identity
  -> a
runIdentityComp comp = runIdentity $ returnVal $ runComp comp idLift Nil

execComp
  :: forall ops lift m a .
  ( Monad m
  , Effects ops
  , OpsConstraint ops m
  , LiftMonoid lift
  , EffFunctor lift (ops)
  )
  => Computation lift ops (Return a) m
  -> m a
execComp comp = returnVal $ runComp comp idLift captureOps
