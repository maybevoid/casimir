{-# Language UndecidableInstances #-}

module Control.Effect.Implicit.Higher.Free
where

import Data.Kind

import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.CoOp
import Control.Effect.Implicit.Higher.EffFunctor
import Control.Effect.Implicit.Higher.ContraLift

import qualified Control.Effect.Implicit.Base as Base
import qualified Control.Effect.Implicit.Freer as Base

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

instance
  {-# OVERLAPPABLE #-}
  ( HigherEffOps ops
  , HigherEffCoOp ops
  , EffCoOp ops
  , Functor (Base.CoOperation ops)
  , Base.FreeOps ops
  )
  => FreeOps ops
  where
    mkFreeOps
      :: forall eff
       . (Effect eff)
      => (forall a
           . HigherCoOp (Base.CoOperation ops) eff a
          -> eff a)
      -> HigherOps (Base.Operation ops) eff eff
    mkFreeOps liftCoOp1 = HigherOps ops
     where
      ops :: Base.Operation ops eff
      ops = Base.mkFreeOps liftCoOp2

      liftCoOp2
        :: forall a
         . Base.CoOperation ops a
        -> eff a
      liftCoOp2 op = liftCoOp1 $ HigherOp op


liftCoOpHandler
  :: forall ops eff f
   . ( Effect eff
     , HigherEffOps ops
     , HigherEffCoOp ops
     )
  => (forall a . Base.CoOpHandler ops a (f a) eff)
  -> ContraFree eff f
  -> CoOpHandler ops eff f
liftCoOpHandler handler1 contraLift =
  CoOpHandler handleReturn handleOps contraLift
  where
    handleReturn :: forall a . a -> eff (f a)
    handleReturn = Base.returnHandler handler1

    handleOps
      :: forall a r
       . HigherCoOp (Base.CoOperation ops) (eff ∘ f) a
      -> (a -> eff (f r))
      -> (eff (f r))
    handleOps (HigherOp coop) cont =
      Base.coOpHandler handler1 coop cont

lowerCoOpHandler
  :: forall ops eff f
   . ( Effect eff
     , HigherEffOps ops
     , HigherEffCoOp ops
     )
  => CoOpHandler ops eff f
  -> (forall a . Base.CoOpHandler ops a (f a) eff)
lowerCoOpHandler
  (CoOpHandler handleReturn1 handleOp1 _) =
  handler2
   where
    handler2 :: forall a . Base.CoOpHandler ops a (f a) eff
    handler2 = Base.CoOpHandler handleReturn1 handleOp2
     where
      handleOp2
        :: forall x
         . Base.CoOperation ops x
        -> (x -> eff (f a))
        -> eff (f a)
      handleOp2 op = handleOp1 $ HigherOp op