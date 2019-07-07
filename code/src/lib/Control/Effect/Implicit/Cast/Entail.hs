{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.Implicit.Cast.Entail
  ( EntailOps (..)
  , EntailOps' (..)
  , type (⊇)
  )
where

import Data.Constraint
import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Cast.Cast

class
  (Effect eff, ImplicitOps ops1, ImplicitOps ops2)
  => EntailOps' ops1 ops2 eff
  where
    entailOps' :: OpsCast' ops1 ops2 eff

instance
  ( Effect eff, ImplicitOps ops1, ImplicitOps ops2
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
  , forall eff . Effect eff => EntailOps' ops1 ops2 eff
  , forall eff . Effect eff => EntailOps' ops1 ops1 eff
  , forall eff . Effect eff => EntailOps' ops2 ops2 eff
  )
  => EntailOps ops1 ops2
  where
    entailOps :: OpsCast ops1 ops2
    entailOps = entailOps' @ops1 @ops2

infixl 6 ⊇
type ops1 ⊇ ops2 = EntailOps ops1 ops2
