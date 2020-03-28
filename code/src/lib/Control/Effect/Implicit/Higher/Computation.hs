
module Control.Effect.Implicit.Higher.Computation
where

import Control.Effect.Implicit.Base.Lift
import Control.Effect.Implicit.Base.Union
import Control.Effect.Implicit.Base.Implicit

import Control.Effect.Implicit.Computation
import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.Free

import qualified Control.Effect.Implicit.Base as Base

type HigherComputation = Computation HigherLiftEff
type HigherOpsHandler ops handler = OpsHandler HigherLiftEff ops handler
type HigherPipeline = Pipeline HigherLiftEff

higherToBaseLiftEff
  :: forall eff1 eff2
   . (Effect eff1, Effect eff2)
  => HigherLiftEff eff1 eff2
  -> LiftEff eff1 eff2
higherToBaseLiftEff higherLift = mkLiftEff $ baseLiftEff higherLift

toHigherComputation
  :: forall ops comp eff
   . (Base.EffOps ops, Effect eff)
  => BaseComputation ops comp eff
  -> HigherComputation ops comp eff
toHigherComputation = strengthenComputation higherToBaseLiftEff

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
     . (ImplicitOps ops2, Base.HigherEffFunctor (Base.Operation ops2))
    => HigherComputation (handler ∪ ops2) (Return a) eff1
    -> HigherComputation (ops1 ∪ ops2) (Return (f a)) eff1
  pipeline comp1 = Computation comp2
   where
    comp2
      :: forall eff2
       . (Effect eff2)
      => HigherLiftEff eff1 eff2
      -> Base.Operation (ops1 ∪ ops2) eff2
      -> Return (f a) eff2
    comp2 lift12 (UnionOps ops1 ops2) =
      Return comp4
     where
      handler2 :: CoOpHandler handler f eff2
      handler2 = runComp handler1 lift12 ops1

      comp3 :: free handler eff2 a
      comp3 = returnVal $ runComp comp1
        (joinLiftEff lift12 freeHigherLiftEff)
        (LowerOps freeOps ∪ applyLiftEff freeHigherLiftEff ops2)

      comp4 :: eff2 (f a)
      comp4 = handleFree handler2 comp3
