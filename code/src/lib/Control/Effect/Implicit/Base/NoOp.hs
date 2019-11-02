
module Control.Effect.Implicit.Base.NoOp
  ( NoEff
  , NoOp (..)
  , NoConstraint
  )
where

import Data.Kind
import Control.Effect.Implicit.Base.Base
import Control.Effect.Implicit.Base.Implicit
import Control.Effect.Implicit.Base.EffFunctor

data NoEff

-- | The trivial effect 'NoOp' have singleton operation and can be trivially
-- satisfied for all 'Effect' @eff@. It is the identity for the
-- @Control.Effect.Implicit.Base.Union@ type operator such that:
--
-- @
-- forall ops . ops 'Control.Effect.Implicit.Base.∪' 'NoOp' ~= 'NoOp' 'Control.Effect.Implicit.Base.∪' ops ~= ops
-- @
--
-- which by @ops1 ~= ops2@ means
-- @ops1 'Control.Effect.Implicit.Computation.⊇' ops2@ and
-- @ops2 'Control.Effect.Implicit.Computation.⊇' ops1@, i.e. they satisfies
-- equivalent set of 'OpsConstraints' up to casting equivalents by
-- 'Control.Effect.Implicit.Computation.OpsCast'.

-- | @'Operation' 'NoOp' eff@ is really just @()@ for all 'Effect' @eff@. We instead define
-- 'NoOp' with phantom type @eff@ so that the injectivity condition for
-- 'Operation' can be satisfied.
data NoOp (eff :: Type -> Type) = NoOp

instance EffOps NoEff where
  type Operation NoEff = NoOp

-- | @'OpsConstraint' 'NoOp' eff@ is just the empty constraint @()@ for all
-- 'Effect' @eff@. We instead define the empty class 'NoConstraint' with
-- trivial instance for all 'Effect' @eff@ so that the injectivity condition
-- for 'OpsConstraint' can be satisfied.
class NoConstraint (eff :: Type -> Type) where

-- | 'NoConstraint' is really just the empty constraint '()' and can be trivially
-- satisfied.
instance NoConstraint eff where

instance EffFunctor NoOp where
  effmap _ _ = NoOp

-- | As the trivial instance for 'ImplicitOps', @'OpsConstraint' 'NoOp'@ does not
-- make use of implicit parameters, as its 'Operation' can be trivially be
-- constructed.
instance ImplicitOps NoEff where
  type OpsConstraint NoEff eff = NoConstraint eff

  withOps _ = id

  captureOps = NoOp
