
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
  )

import Casimir.Computation
import Casimir.Higher.Base
import Casimir.Higher.Free

import qualified Casimir.Base as Base

type HigherComputation = Computation HigherLift
type HigherOpsHandler ops handler = OpsHandler HigherLift ops handler
type HigherPipeline = Pipeline HigherLift

higherToBaseLift
  :: forall eff1 eff2
   . (Effect eff1, Effect eff2)
  => HigherLift eff1 eff2
  -> Lift eff1 eff2
higherToBaseLift higherLift = Lift $ hlBaseLift higherLift

toHigherComputation
  :: forall ops comp eff
   . (Base.EffOps ops, Effect eff)
  => BaseComputation ops comp eff
  -> HigherComputation ops comp eff
toHigherComputation = strengthenComputation higherToBaseLift

{-# INLINE coopHandlerToPipeline #-}
coopHandlerToPipeline
  :: forall free ops1 handler eff1 f a .
  ( Functor f
  , Effect eff1
  , Base.EffOps ops1
  , EffOps handler
  , ImplicitOps handler
  , LowerEffOps handler
  , FreeOps handler
  , FreeHandler free
  )
  => HigherComputation ops1 (CoOpHandler handler f) eff1
  -> HigherPipeline ops1 handler (Return a) (Return (f a)) eff1 eff1
coopHandlerToPipeline handler1 = Pipeline pipeline
 where
  pipeline
    :: forall ops2
     . ( ImplicitOps ops2
       , EffFunctor HigherLift (Base.Operation ops2))
    => HigherComputation (handler ∪ ops2) (Return a) eff1
    -> HigherComputation (ops1 ∪ ops2) (Return (f a)) eff1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall eff2
       . (Effect eff2)
      => HigherLift eff1 eff2
      -> Base.Operation (ops1 ∪ ops2) eff2
      -> Return (f a) eff2
    comp2 lift12 (UnionOps ops1 ops2) =
      Return comp4
     where
      handler2 :: CoOpHandler handler f eff2
      handler2 = runComp handler1 lift12 ops1

      comp3 :: free handler eff2 a
      comp3 = returnVal $ runComp comp1
        (joinLift lift12 freeHigherLift)
        (LowerOps freeOps ∪ effmap freeHigherLift ops2)

      comp4 :: eff2 (f a)
      comp4 = handleFree handler2 comp3
