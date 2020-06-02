
module Casimir.Cast.Cast
  -- ( OpsCast
  -- , OpsCast'
  -- , cast
  -- , mergeDict
  -- , withCast
  -- , castOps
  -- , extendCast
  -- , composeCast
  -- )
where

-- import Data.Constraint

-- import Casimir.Base

-- type OpsCast' ops1 ops2 m =
--   (EffConstraint ops1 m) => Dict (OpsConstraint ops2 m)

-- type OpsCast ops1 ops2 =
--   forall m . OpsCast' ops1 ops2 m

-- cast :: forall p . p => Dict p
-- cast = Dict

-- mergeDict
--   :: forall p q
--    . Dict p
--   -> Dict q
--   -> Dict (p, q)
-- mergeDict Dict Dict = Dict

-- withCast
--   :: forall m ops1 ops2 r
--    . ( EffConstraint ops1 m )
--   => OpsCast ops1 ops2
--   -> (OpsConstraint ops2 m => r)
--   -> r
-- withCast caster res =
--   case caster @m of
--     Dict -> res

-- castOps
--   :: forall m ops1 ops2 .
--   ( Monad m
--   , ImplicitOps ops1
--   , ImplicitOps ops2
--   )
--   => OpsCast ops1 ops2
--   -> Operation ops1 m
--   -> Operation ops2 m
-- castOps caster ops = withOps ops $
--   withCast @m @ops1 @ops2
--     caster captureOps

-- extendCast
--   :: forall ops1 ops2 ops3 .
--   ( ImplicitOps ops1
--   , ImplicitOps ops2
--   , ImplicitOps ops3
--   )
--   => OpsCast ops1 ops2
--   -> OpsCast (ops1 ∪ ops3) (ops2 ∪ ops3)
-- extendCast caster1 = caster2
--  where
--   caster2
--     :: forall m .
--     (EffConstraint (ops1 ∪ ops3) m)
--     => Dict (OpsConstraint (ops2 ∪ ops3) m)
--   caster2 = case caster1 @m of
--     Dict -> Dict

-- composeCast
--   :: forall ops1 ops2 ops3.
--   ( ImplicitOps ops1
--   , ImplicitOps ops2
--   , ImplicitOps ops3
--   )
--   => OpsCast ops1 ops2
--   -> OpsCast ops2 ops3
--   -> OpsCast ops1 ops3
-- composeCast cast1 cast2 = cast3
--   where
--     cast3
--       :: forall m .
--       (EffConstraint ops1 m)
--       => Dict (OpsConstraint ops3 m)
--     cast3 = withCast @m @ops1 @ops2 cast1 $
--       withCast @m @ops2 @ops3 cast2 Dict
