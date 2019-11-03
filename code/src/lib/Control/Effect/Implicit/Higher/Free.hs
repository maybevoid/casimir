module Control.Effect.Implicit.Higher.Free
where

import Data.Kind

import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.CoOp
import Control.Effect.Implicit.Higher.EffFunctor
import Control.Effect.Implicit.Higher.ContraLift

newtype Nest f g a = Nest {
  unNest :: f (g a)
}

type f ∘ g = Nest f g

instance
  (Functor f, Functor g)
  => Functor (Nest f g)
   where
    fmap f (Nest mx) = Nest $ fmap (fmap f) mx

data CoOpHandler
  ops
  (eff :: Type -> Type)
  (f :: Type -> Type)
  = CoOpHandler
    { returnHandler
        :: forall a . a -> eff (f a)

    , operationHandler
        :: forall a r
        . CoOperation ops (eff ∘ f) a
        -> (a -> eff (f r))
        -> eff (f r)

    , contraLiftHandler
        :: ContraFree eff f
    }

class
  ( EffCoOp ops
  , CoOpFunctor (CoOperation ops)
  , EffFunctor (Operation ops)
  )
  => FreeOps ops
   where
    mkFreeOps
      :: forall eff
      . (Effect eff)
      => (forall a . CoOperation ops eff a -> eff a)
      -> Operation ops eff eff

class
  ( forall ops eff
    . (FreeOps ops, Effect eff)
    => Effect (free ops eff)
  )
  => FreeEff free
   where
    freeOps :: forall ops eff
       . (FreeOps ops, Effect eff)
      => Operation ops (free ops eff) (free ops eff)

    liftFree :: forall ops eff a
       . (FreeOps ops, Effect eff)
      => eff a
      -> free ops eff a

class
  (FreeEff free)
  => FreeHandler free
   where
    freeContraLift
      :: forall eff ops
       . ( Effect eff
         , FreeOps ops
         )
      => ContraLift eff (free ops eff)

    handleFree
      :: forall ops eff f a
       . ( Effect eff
         , FreeOps ops
         , Functor f
         )
      => CoOpHandler ops eff f
      -> free ops eff a
      -> eff (f a)
