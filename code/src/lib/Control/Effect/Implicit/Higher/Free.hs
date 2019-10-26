module Control.Effect.Implicit.Higher.Free

where

import Data.Kind
import Control.Monad.Identity

import Control.Effect.Implicit.Base
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

type ContraFree eff f =
  forall a
   . (forall w
       . (Functor w)
     => (forall x . f (eff x) -> eff (w x))
     -> eff (w (eff (f a))))
  -> eff (f a)

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
  , forall f
     . (Functor f)
    => Functor (CoOperation ops f)
  )
  => CoOpFunctor ops
  where
    liftCoOp
      :: forall f1 f2 a
       . (Functor f1, Functor f2)
      => (forall x . f1 x -> f2 x)
      -> CoOperation ops f1 a
      -> CoOperation ops f2 a

class
  ( EffCoOp ops
  , CoOpFunctor ops
  , HEffFunctor ops
  )
  => FreeOps ops
   where
    mkFreeOps
      :: forall eff
      . (Effect eff)
      => (forall a . CoOperation ops eff a -> eff a)
      -> ops eff eff

class
  ( forall ops eff
    . (FreeOps ops, Effect eff)
    => Effect (free ops eff)
  )
  => FreeEff free
   where
    freeOps :: forall ops eff
       . (FreeOps ops, Effect eff)
      => ops (free ops eff) (free ops eff)

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

data CoState s eff a = CoState {
  runCoState :: s -> eff (s, a)
} deriving (Functor)

contraIdentity
  :: forall eff
   . (Effect eff)
  => ContraFree eff Identity
contraIdentity = handler1
 where
  handler1
    :: forall a
     . ((forall x . Identity (eff x) -> eff (Identity x))
        -> eff (Identity (eff (Identity a))))
    -> eff (Identity a)
  handler1 cont1 = cont1 contra1 >>= runIdentity

  contra1 :: forall a . Identity (eff a) -> eff (Identity a)
  contra1 (Identity mx) = mx >>= return . Identity

contraState
  :: forall s eff
   . (Effect eff)
  => ContraFree eff (CoState s eff)
contraState = handler1
 where
  handler1
    :: forall a
     . ((forall x . CoState s eff (eff x) -> eff (s, x))
        -> eff (s, eff (CoState s eff a)))
    -> eff (CoState s eff a)
  handler1 cont1 = return $ CoState handler2
   where
    handler2 :: s -> eff (s, a)
    handler2 s1 = do
      (s2, cont2) <- cont1 handler3
      cont3 <- cont2
      runCoState cont3 s2
     where
      handler3 :: CoState s eff (eff x) -> eff (s, x)
      handler3 (CoState cont3) = do
        (s2, mx) <- cont3 s1
        x <- mx
        return (s2, x)
