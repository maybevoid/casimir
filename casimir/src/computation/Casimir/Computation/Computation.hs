{-# LANGUAGE PolyKinds #-}

module Casimir.Computation.Computation
  ( BaseComputation
  , Computation (..)
  , liftComputation
  , OpsHandler
  , BaseOpsHandler
  , strengthenComputation
  )
where

import Data.Kind
import Casimir.Base

newtype Computation
  (lift :: (Type -> Type) -> (Type -> Type) -> Type)
  (ops :: (Type -> Type) -> Type)
  (comp :: (Type -> Type) -> Type)
  (m1 :: Type -> Type)
  = Computation
    { runComp
        :: forall m2
         . ( Effects ops
           , Monad m2
           )
        => lift m1 m2
        -> ops m2
        -> comp m2
    }

type BaseComputation = Computation Lift

type OpsHandler lift ops handler = Computation lift ops (handler)
type BaseOpsHandler ops handler = OpsHandler Lift ops handler

instance
  ( Effects ops
  , LiftMonoid lift
  , EffFunctor lift (ops)
  )
  => EffFunctor lift (Computation lift ops comp) where
    effmap = liftComputation

liftComputation
  :: forall lift ops comp m1 m2
   . ( Effects ops
     , Monad m1
     , Monad m2
     , LiftMonoid lift
     )
  => lift m1 m2
  -> Computation lift ops comp m1
  -> Computation lift ops comp m2
liftComputation lift12 comp1 = Computation comp2
  where
    comp2 :: forall m3 .
      ( Monad m3
      )
      => lift m2 m3
      -> ops m3
      -> comp m3
    comp2 lift23 = runComp comp1 $ joinLift lift12 lift23

strengthenComputation
  :: forall lift1 lift2 ops comp m
   . ( Effects ops
     , Monad m
     , LiftMonoid lift1
     , LiftMonoid lift2
     )
  => (forall m1 m2
       . (Monad m1, Monad m2)
      => lift2 m1 m2
      -> lift1 m1 m2)
  -> Computation lift1 ops comp m
  -> Computation lift2 ops comp m
strengthenComputation strengthenLift comp1 =
  Computation comp2
 where
  comp2 :: forall m2 .
    ( Monad m2
    )
    => lift2 m m2
    -> ops m2
    -> comp m2
  comp2 lift = runComp comp1 $ strengthenLift lift
