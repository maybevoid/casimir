{-# language PolyKinds #-}

module Casimir.Higher.Computation
where

import Casimir.Base
  ( Lift (..)
  , HigherLift (..)
  , EffFunctor (..)
  , LiftMonoid (..)
  , Union (..)
  )

import Casimir.Computation
import Casimir.Higher.Free

import Casimir.Higher.Base
  ( Effects
  , LowerOps (..)
  )

import qualified Casimir.Base as Base

type HigherPipeline = Pipeline HigherLift
type HigherComputation = Computation HigherLift
type HigherOpsHandler ops handler = OpsHandler HigherLift ops handler

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
  :: forall free ops1 ops2 m1 f a
   . ( Functor f
     , Monad m1
     , Base.Effects ops1
     , Base.Effects (LowerOps ops2)
     , Effects ops2
     , FreeOps ops2
     , FreeHandler free
     )
  => HigherComputation ops1 (CoOpHandler ops2 f) m1
  -> HigherPipeline ops1 (LowerOps ops2) (Return a) (Return (f a)) m1 m1
coopHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline
    :: forall ops3
     . ( Base.Effects ops3
       , EffFunctor HigherLift ops3
       )
    => HigherComputation (Union (LowerOps ops2) ops3) (Return a) m1
    -> HigherComputation (Union ops1 ops3) (Return (f a)) m1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall m2
       . (Monad m2)
      => HigherLift m1 m2
      -> Union ops1 ops3 m2
      -> Return (f a) m2
    comp2 lift12 (Union ops1 ops2) =
      Return comp4
     where
      handler2 :: CoOpHandler ops2 f m2
      handler2 = runComp handler1 lift12 ops1

      comp3 :: free ops2 m2 a
      comp3 = returnVal $ runComp comp1
        (joinLift lift12 freeHigherLift)
        (Union (LowerOps freeOps) (effmap freeHigherLift ops2))

      comp4 :: m2 (f a)
      comp4 = handleFree handler2 comp3
