
module Control.Effect.Implicit.Higher.CoOp
where

import Data.Kind

class EffCoOp ops where
  type family CoOperation ops =
    ( coop
      :: (Type -> Type)
      -> Type
      -> Type
    ) | coop -> ops

class
  ( EffCoOp ops
  )
  => CoOpFunctor ops
  where
    liftCoOp
      :: forall f1 f2 a
       . (Functor f1, Functor f2)
      => (forall x . f1 x -> f2 x)
      -> CoOperation ops f1 a
      -> CoOperation ops f2 a

    -- GHC is unable to deduce the following
    -- even if we add it as quantified constraints:
    --
    --  forall f
    --   . (Functor f)
    --  => Functor (CoOperation ops f)
    mapCoOp
      :: forall f a b
       . (Functor f)
      => (a -> b)
      -> CoOperation ops f a
      -> CoOperation ops f b
