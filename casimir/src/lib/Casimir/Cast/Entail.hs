{-# LANGUAGE UndecidableInstances #-}

module Casimir.Cast.Entail
  ( EntailOps (..)
  , EntailOps' (..)
  , type (⊇)
  )
where

import Data.Constraint
import Casimir.Base
import Casimir.Cast.Cast

class
  (Monad eff, ImplicitOps ops1, ImplicitOps ops2)
  => EntailOps' ops1 ops2 eff
  where
    entailOps' :: OpsCast' ops1 ops2 eff

instance
  ( Monad eff, ImplicitOps ops1, ImplicitOps ops2
  , (p ~ OpsConstraint ops1 eff, q ~ OpsConstraint ops2 eff)
  , (p => q)
  )
  => EntailOps' ops1 ops2 eff
  where
    entailOps' :: OpsCast' ops1 ops2 eff
    entailOps' = Dict

class
  (ImplicitOps ops1, ImplicitOps ops2)
  => EntailOps ops1 ops2
  where
    entailOps :: OpsCast ops1 ops2

instance
  ( ImplicitOps ops1, ImplicitOps ops2
  , forall eff . Monad eff => EntailOps' ops1 ops2 eff
  , forall eff . Monad eff => EntailOps' ops1 ops1 eff
  , forall eff . Monad eff => EntailOps' ops2 ops2 eff
  )
  => EntailOps ops1 ops2
  where
    entailOps :: OpsCast ops1 ops2
    entailOps = entailOps' @ops1 @ops2

infixl 6 ⊇
type ops1 ⊇ ops2 = EntailOps ops1 ops2
