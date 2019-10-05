module Control.Effect.Implicit.Higher.Free

where

import Data.Kind
import Control.Monad.Identity

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.EffFunctor
import Control.Effect.Implicit.Higher.ContraLift

class EffCoOp
  (ops :: (Type -> Type) -> (Type -> Type) -> Type)
   where
    type family CoOperation ops =
      ( coop
        :: (Type -> Type)
        -> Type
        -> Type
      ) | coop -> ops

data CoOpHandler
  (ops :: (Type -> Type) -> (Type -> Type) -> Type)
  (f :: Type -> Type)
  = CoOpHandler
    { returnHandler
        :: forall a . a -> f a

    , coOpHandler
        :: forall a
        . CoOperation ops f a
        -> f a
    }

class CoOpFunctor ops where
  mapCoOpHandler
    :: forall f1 f2
     . Monad f2
    => (forall x . f1 x -> f2 x)
    -> ContraLiftEff f1 f2
    -> CoOpHandler ops f1
    -> CoOpHandler ops f2
