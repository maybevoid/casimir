{-# LANGUAGE UndecidableInstances #-}

module Casimir.Base.Implicit
  ( ImplicitOps (..)
  , EffConstraint
  , Eff
  )
where

import Data.Kind

import Casimir.Base.EffOps

-- | 'ImplicitOps' gives computations access to mect operations of an
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
-- The definition for 'ImplicitOps' for most mect operations can typically
-- be derived mechanically. We may look into using template Haskell to generate
-- instances for 'ImplicitOps' in future to reduce some boilerplate.
class
  (EffOps ops)
  => ImplicitOps ops where

    -- | The constraint kind for the mect operation under 'Monad' @m@.
    -- This is typically an implicit parameter with a unique name, e.g.
    -- @type OpsConstraint FooOps m = (?fooOps :: FooOps m)@.
    --
    -- Note that there is a injective type families condition, and given that
    -- implicit parameters have a single namespace, users must come out with
    -- naming conventions for their custom mects to avoid name clashing
    -- that would result in compile-time injectivity violation error.
    type family OpsConstraint ops (m :: Type -> Type)
      = (c :: Constraint) | c -> ops m

    -- | Takes an mect operation @'Operation' ops m@ and bind it to the
    -- implicit parameter specified in @'OpsConstraint' ops m@ for the
    -- continuation @r@. The expression @r@ can then use the mect operations
    -- without having to explicitly pass them around as function arguments.
    -- For the example @FooEff@, the body for 'withOps' can be defined as
    -- @withOps fooOps cont = let ?fooOps = fooOps in cont @.
    withOps
      :: forall m r
       . (Monad m)
      => Operation ops m
      -> (OpsConstraint ops m => r)
      -> r

    -- | If an implicit parameter for the mect operation is available in the
    -- context, capture it and return the operation as a value. For the example
    -- @FooEff@, the body for 'captureOps' can be defined as @captureOps = ?fooOps@.
    captureOps
      :: forall m
       . (Monad m, OpsConstraint ops m)
      => Operation ops m

-- | This is a type alias for the implicit parameter constraint for @ops@,
-- in addition to requiring @m@ to be an 'Monad'. This helps reducing
-- boilerplate in generic computations so that we do not have to keep
-- repeating the @(Monad m)@ constraint in our type signatures.
type EffConstraint ops m = (Monad m, OpsConstraint ops m)

type Eff ops a = forall m . (EffConstraint ops m) => m a
