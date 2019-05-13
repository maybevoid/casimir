
module Control.Effect.Implicit.Computation.Value
  ( FunctorComp (..)
  , Return (..)
  , Arrow (..)
  , ReturnCtx (..)
  , GenericReturn
  , IdentityComputation
  , genericComputation
  , genericReturn
  , runIdentityComp
  , execComp
  , arrowComputation
  )
where

import Data.Kind
import Control.Monad.Identity (Identity (..))

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation.Computation

class FunctorComp ret where
  mapComp
    :: forall a b eff
     . (Effect eff)
    => (a -> b)
    -> ret a eff
    -> ret b eff

newtype Return a eff = Return {
  returnVal :: eff a
}

newtype Arrow a ret b (eff :: Type -> Type) = Arrow {
  arrowVal :: a -> ret b eff
}

newtype ReturnCtx f ret a (eff :: Type -> Type) = ReturnCtx {
  returnCtx :: ret (f a) eff
}

instance EffFunctor (Return a) where
  effmap lifter (Return mx) = Return $ lifter mx

instance (EffFunctor (ret b)) => EffFunctor (Arrow a ret b) where
  effmap lifter (Arrow fn) = Arrow $
    \x -> effmap lifter $ fn x

instance
  (EffFunctor (ret (f a)))
  => EffFunctor (ReturnCtx f ret a)
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

type GenericReturn ops a =
  forall eff . (Effect eff) => Computation ops (Return a) eff

type IdentityComputation a = Computation NoEff (Return a) Identity

{-# INLINE genericComputation #-}
genericComputation
  :: forall ops comp
   . (ImplicitOps ops)
  => (forall eff
        . (EffConstraint ops eff)
       => comp eff)
  -> (forall eff . (Effect eff) => Computation ops comp eff)
genericComputation comp = Computation $
  \ _ ops -> withOps ops comp

{-# INLINE genericReturn #-}
genericReturn
  :: forall ops a
   . (ImplicitOps ops)
  => (forall eff
        . (EffConstraint ops eff)
       => eff a)
  -> GenericReturn ops a
genericReturn comp = genericComputation $ Return comp

arrowComputation
  :: forall ops a b
   . (ImplicitOps ops)
  => (forall eff
        . (EffConstraint ops eff)
       => a
       -> eff b)
  -> (forall eff . (Effect eff)
       => Computation ops (Arrow a Return b) eff)
arrowComputation fn = genericComputation $ Arrow $
  \x -> Return $ fn x

runIdentityComp :: forall a . IdentityComputation a -> a
runIdentityComp comp = runIdentity $ returnVal $ runComp comp idLift NoOp

execComp
  :: forall ops eff a .
  ( ImplicitOps ops
  , EffConstraint ops eff
  )
  => Computation ops (Return a) eff
  -> eff a
execComp comp = returnVal $ runComp comp idLift captureOps