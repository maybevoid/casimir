{-# LANGUAGE UndecidableInstances #-}

module Casimir.Cast.Entail
  -- ( EntailOps (..)
  -- , EntailOps' (..)
  -- , type (⊇)
  -- )
where

-- import Data.Constraint
-- import Casimir.Base
-- import Casimir.Cast.Cast

-- class
--   (Monad m, ImplicitOps ops1, ImplicitOps ops2)
--   => EntailOps' ops1 ops2 m
--   where
--     entailOps' :: OpsCast' ops1 ops2 m

-- instance
--   ( Monad m, ImplicitOps ops1, ImplicitOps ops2
--   , (p ~ OpsConstraint ops1 m, q ~ OpsConstraint ops2 m)
--   , (p => q)
--   )
--   => EntailOps' ops1 ops2 m
--   where
--     entailOps' :: OpsCast' ops1 ops2 m
--     entailOps' = Dict

-- class
--   (ImplicitOps ops1, ImplicitOps ops2)
--   => EntailOps ops1 ops2
--   where
--     entailOps :: OpsCast ops1 ops2

-- instance
--   ( ImplicitOps ops1, ImplicitOps ops2
--   , forall m . Monad m => EntailOps' ops1 ops2 m
--   , forall m . Monad m => EntailOps' ops1 ops1 m
--   , forall m . Monad m => EntailOps' ops2 ops2 m
--   )
--   => EntailOps ops1 ops2
--   where
--     entailOps :: OpsCast ops1 ops2
--     entailOps = entailOps' @ops1 @ops2

-- infixl 6 ⊇
-- type ops1 ⊇ ops2 = EntailOps ops1 ops2
