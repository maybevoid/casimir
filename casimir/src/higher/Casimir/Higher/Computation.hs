
module Casimir.Higher.Computation
where

import Casimir.Base
  ( Lift (..)
  , HigherLift (..)
  , EffFunctor (..)
  , ImplicitOps
  , LiftMonoid (..)
  , pattern Union
  , type (∪)
  , (∪)
  )

import Casimir.Computation
import Casimir.Higher.Base
import Casimir.Higher.Free

import qualified Casimir.Base as Base

type HigherComputation = Computation HigherLift
type HigherOpsHandler ops handler = OpsHandler HigherLift ops handler
type HigherPipeline = Pipeline HigherLift

higherToBaseLift
  :: forall m1 m2
   . (Monad m1, Monad m2)
  => HigherLift m1 m2
  -> Lift m1 m2
higherToBaseLift higherLift = Lift $ hlBaseLift higherLift

toHigherComputation
  :: forall ops comp m
   . (Base.Effects ops, Monad m)
  => BaseComputation ops comp m
  -> HigherComputation ops comp m
toHigherComputation = strengthenComputation higherToBaseLift

{-# INLINE coopHandlerToPipeline #-}
coopHandlerToPipeline
  :: forall free eff1 eff2 ops2 m1 f a
   . ( Functor f
     , Monad m1
     , Base.Effects eff1
     , Effects eff2
     , Base.Effects eff2
     , FreeOps ops2
     , ImplicitOps eff2
     , FreeHandler free
     , Operations eff2 ~ ops2
     , Base.Operations eff2 ~ LowerOps ops2
     )
  => HigherComputation eff1 (CoOpHandler ops2 f) m1
  -> HigherPipeline eff1 eff2 (Return a) (Return (f a)) m1 m1
coopHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline
    :: forall eff3 ops3
     . ( ImplicitOps eff3
       , Base.Operations eff3 ~ ops3
       , EffFunctor HigherLift ops3
       )
    => HigherComputation (eff2 ∪ eff3) (Return a) m1
    -> HigherComputation (eff1 ∪ eff3) (Return (f a)) m1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall m2
       . (Monad m2)
      => HigherLift m1 m2
      -> Base.Operations (eff1 ∪ eff3) m2
      -> Return (f a) m2
    comp2 lift12 (Union ops1 ops2) =
      Return comp4
     where
      handler2 :: CoOpHandler ops2 f m2
      handler2 = runComp handler1 lift12 ops1

      comp3 :: free ops2 m2 a
      comp3 = returnVal $ runComp comp1
        (joinLift lift12 freeHigherLift)
        (LowerOps freeOps ∪ effmap freeHigherLift ops2)

      comp4 :: m2 (f a)
      comp4 = handleFree handler2 comp3
