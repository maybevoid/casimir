{-# LANGUAGE UndecidableInstances #-}

module Casimir.Base.Implicit
  ( ImplicitOps (..)
  , EffConstraint
  , Eff
  )
where

import Data.Kind

import Casimir.Base.EffOps
import Casimir.Base.Effect

-- | 'ImplicitOps' gives computations access to effect operations of an
-- operation through implicit parameter constraints. It hides the machinery
-- of implicit parameters and make them appear like regular constraints except
-- with local scope.
--
-- The law for 'ImplicitOps' is
--
-- @
-- forall ops . 'withOps' ops 'captureOps' === ops
-- @
--
-- This means any non-trivial instance for 'ImplicitOps' must somehow make use of
-- implicit parameters for the law to hold.
--
-- The definition for 'ImplicitOps' for most effect operations can typically
-- be derived mechanically. We may look into using template Haskell to generate
-- instances for 'ImplicitOps' in future to reduce some boilerplate.
class
  (EffOps ops)
  => ImplicitOps ops where

    -- | The constraint kind for the effect operation under 'Effect' @eff@.
    -- This is typically an implicit parameter with a unique name, e.g.
    -- @type OpsConstraint FooOps eff = (?fooOps :: FooOps eff)@.
    --
    -- Note that there is a injective type families condition, and given that
    -- implicit parameters have a single namespace, users must come out with
    -- naming conventions for their custom effects to avoid name clashing
    -- that would result in compile-time injectivity violation error.
    type family OpsConstraint ops (eff :: Type -> Type)
      = (c :: Constraint) | c -> ops eff

    -- | Takes an effect operation @'Operation' ops eff@ and bind it to the
    -- implicit parameter specified in @'OpsConstraint' ops eff@ for the
    -- continuation @r@. The expression @r@ can then use the effect operations
    -- without having to explicitly pass them around as function arguments.
    -- For the example @FooEff@, the body for 'withOps' can be defined as
    -- @withOps fooOps cont = let ?fooOps = fooOps in cont @.
    withOps
      :: forall eff r
       . (Effect eff)
      => Operation ops eff
      -> (OpsConstraint ops eff => r)
      -> r

    -- | If an implicit parameter for the effect operation is available in the
    -- context, capture it and return the operation as a value. For the example
    -- @FooEff@, the body for 'captureOps' can be defined as @captureOps = ?fooOps@.
    captureOps
      :: forall eff
       . (Effect eff, OpsConstraint ops eff)
      => Operation ops eff

-- | This is a type alias for the implicit parameter constraint for @ops@,
-- in addition to requiring @eff@ to be an 'Effect'. This helps reducing
-- boilerplate in generic computations so that we do not have to keep
-- repeating the @(Effect eff)@ constraint in our type signatures.
type EffConstraint ops eff = (Effect eff, OpsConstraint ops eff)

type Eff ops a = forall eff . (EffConstraint ops eff) => eff a
