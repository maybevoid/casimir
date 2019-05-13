
module Control.Effect.Implicit.Base.Lift
  ( LiftEff
  , idLift
  , mkLiftEff
  , liftEff
  , applyEffmap
  , joinLift
  )
where

import Data.Kind

import Control.Effect.Implicit.Base.Effect
import Control.Effect.Implicit.Base.EffFunctor

-- | An opaque object-ish effect lifter that can apply 'effmap' to an 'EffFunctor'.
-- We define dedicated datatype instead of using the natural transformation
-- @(~>)@ so that in the case of identity, we can construct a special 'idLift'
-- that skip calling @'effmap' id@ and return the original 'EffFunctor'. This may
-- save a bit on creating redundant
-- 'Control.Effect.Implicit.Base.Operation' and
-- 'Control.Effect.Implicit.Base.CoOperation' objects during @'effmap' id@,
-- particularly when they are used together with
-- 'Control.Effect.Implicit.Computation.Computation'.
data LiftEff (eff1 :: (Type -> Type)) (eff2 :: (Type -> Type))
  = MkLiftEff {

    -- | The base natural transformation that can be extracted
    liftEff :: forall x . eff1 x -> eff2 x,

    -- | When applying effect lifting to an 'EffFunctor', 'applyEffmap' should
    -- be used instead of calling 'effmap' directly. This could potentially
    -- save some unnecesssary objects recreation in the case of 'idLift'.
    applyEffmap
      :: forall comp
      . (EffFunctor comp)
      => comp eff1
      -> comp eff2,

    -- | An optimizable composition of two natural transformations. This makes sure
    -- the no new 'LiftEff' is created when either side is 'idLift'. The behavior
    -- for 'joinLift' is as follow:
    --
    -- @
    --   'joinLift' idLift idLift === idLift
    --   'joinLift' idLift lift2 === lift2
    --   'joinLift' lift1 idLift === lift1
    --   'joinLift' lift1 lift2 === 'mkLiftEff' $ (liftEff lift2) . (liftEff lift1)
    -- @
    joinLift
      :: forall eff3
      . (Effect eff3)
      => LiftEff eff2 eff3
      -> LiftEff eff1 eff3,

    -- | Auxliary helper used by 'joinLift' so that @joinLift lift1 idLift@
    -- would return @lift1@.
    rightJoinLift
      :: forall eff0
      . (Effect eff0)
      => LiftEff eff0 eff1
      -> LiftEff eff0 eff2
  }

-- | Create a 'LiftEff' from a natural transformation @eff1 ~> eff2@. This assumes
-- @eff1@ and @eff2@ are different, as otherwise we can use 'idLift' for optimized
-- version of 'LiftEff'.
mkLiftEff
  :: forall eff1 eff2
   . (Effect eff1, Effect eff2)
  => (forall x . eff1 x -> eff2 x)
  -> LiftEff eff1 eff2
mkLiftEff lifter1 = lifter2
 where
  lifter2 = MkLiftEff {
    liftEff = lifter1,

    applyEffmap = effmap lifter1,

    joinLift = \lifter3 ->
      rightJoinLift lifter3 lifter2,

    rightJoinLift = \lifter3 ->
      mkLiftEff (lifter1 . liftEff lifter3)
  }

-- | An optimized version of 'LiftEff' when there is no effect lifting required,
-- e.g. when we want to execute a
-- 'Control.Effect.Implicit.Computation.Computation' with its original effect.
idLift
  :: forall eff . (Effect eff)
  => LiftEff eff eff
idLift = MkLiftEff id id id id
