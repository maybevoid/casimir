
module Casimir.Base.NoOp
  ( NoEff
  , NoOp (..)
  , NoConstraint
  )
where

import Data.Kind
import Casimir.Base.Lift
import Casimir.Base.EffOps
import Casimir.Base.Implicit
import Casimir.Base.EffFunctor

data NoEff

-- | The trivial mect 'NoOp' have singleton operation and can be trivially
-- satisfied for all 'Monad' @m@. It is the identity for the
-- @Casimir.Base.Union@ type operator such that:
--
-- @
-- forall ops . ops 'Casimir.Base.∪' 'NoOp' ~= 'NoOp' 'Casimir.Base.∪' ops ~= ops
-- @
--
-- which by @ops1 ~= ops2@ means
-- @ops1 'Casimir.Computation.⊇' ops2@ and
-- @ops2 'Casimir.Computation.⊇' ops1@, i.e. they satisfies
-- equivalent set of 'OpsConstraints' up to casting equivalents by
-- 'Casimir.Computation.OpsCast'.

-- | @'Operation' 'NoOp' m@ is really just @()@ for all 'Monad' @m@. We instead define
-- 'NoOp' with phantom type @m@ so that the injectivity condition for
-- 'Operation' can be satisfied.
data NoOp (m :: Type -> Type) = NoOp

instance EffOps NoEff where
  type Operation NoEff = NoOp

-- | @'OpsConstraint' 'NoOp' m@ is just the empty constraint @()@ for all
-- 'Monad' @m@. We instead define the empty class 'NoConstraint' with
-- trivial instance for all 'Monad' @m@ so that the injectivity condition
-- for 'OpsConstraint' can be satisfied.
class NoConstraint (m :: Type -> Type) where

-- | 'NoConstraint' is really just the empty constraint '()' and can be trivially
-- satisfied.
instance NoConstraint m where

instance EffFunctor Lift NoOp where
  effmap _ _ = NoOp

-- | As the trivial instance for 'ImplicitOps', @'OpsConstraint' 'NoOp'@ does not
-- make use of implicit parameters, as its 'Operation' can be trivially be
-- constructed.
instance ImplicitOps NoEff where
  type OpsConstraint NoEff m = NoConstraint m

  withOps _ = id

  captureOps = NoOp
