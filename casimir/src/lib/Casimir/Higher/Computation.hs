
module Casimir.Higher.Computation
where

import Casimir.Base
  ( Lift (..)
  , HigherLift (..)
  , EffFunctor (..)
  , ImplicitOps
  , UnionOps (..)
  , LiftMonoid (..)
  , type (∪)
  , (∪)
  , pattern UnionOps
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
   . (Base.EffOps ops, Monad m)
  => BaseComputation ops comp m
  -> HigherComputation ops comp m
toHigherComputation = strengthenComputation higherToBaseLift

{-# INLINE coopHandlerToPipeline #-}
coopHandlerToPipeline
  :: forall free ops1 handler m1 f a .
  ( Functor f
  , Monad m1
  , Base.EffOps ops1
  , EffOps handler
  , ImplicitOps handler
  , LowerEffOps handler
  , FreeOps handler
  , FreeHandler free
  )
  => HigherComputation ops1 (CoOpHandler handler f) m1
  -> HigherPipeline ops1 handler (Return a) (Return (f a)) m1 m1
coopHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline
    :: forall ops2
     . ( ImplicitOps ops2
       , EffFunctor HigherLift (Base.Operation ops2))
    => HigherComputation (handler ∪ ops2) (Return a) m1
    -> HigherComputation (ops1 ∪ ops2) (Return (f a)) m1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall m2
       . (Monad m2)
      => HigherLift m1 m2
      -> Base.Operation (ops1 ∪ ops2) m2
      -> Return (f a) m2
    comp2 lift12 (UnionOps ops1 ops2) =
      Return comp4
     where
      handler2 :: CoOpHandler handler f m2
      handler2 = runComp handler1 lift12 ops1

      comp3 :: free handler m2 a
      comp3 = returnVal $ runComp comp1
        (joinLift lift12 freeHigherLift)
        (LowerOps freeOps ∪ effmap freeHigherLift ops2)

      comp4 :: m2 (f a)
      comp4 = handleFree handler2 comp3
