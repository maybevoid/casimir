
module Casimir.Higher.CoOp
where

import Data.Kind

import Casimir.Base
  ( type (~>)
  )

import qualified Casimir.Freer as Base

data HigherCoOp coop (f :: Type -> Type) a =
  HigherOp (coop a)

-- class EffCoOp ops where
--   type family CoOperation ops =
--     ( coop
--       :: (Type -> Type)
--       -> Type
--       -> Type
--     ) | coop -> ops

class CoOpFunctor coop where
  liftCoOp
    :: forall f1 f2 a
     . (Functor f1, Functor f2)
    => f1 ~> f2
    -> coop f1 a
    -> coop f2 a

-- class
--   ( EffCoOp ops
--   , Base.EffCoOp ops
--   , CoOperation ops ~ HigherCoOp (Base.CoOperation ops)
--   )
--   => HigherEffCoOp ops

instance
  (Functor coop)
  => Functor (HigherCoOp coop f)
  where
    fmap f (HigherOp ops) = HigherOp $ fmap f ops

instance
  (Functor coop)
  => CoOpFunctor (HigherCoOp coop)
  where
    liftCoOp _ (HigherOp ops) = (HigherOp ops)
