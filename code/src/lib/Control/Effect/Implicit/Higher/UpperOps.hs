{-# Language UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Effect.Implicit.Higher.UpperOps
where

import Data.Kind

import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.CoOp
import Control.Effect.Implicit.Higher.Free
import Control.Effect.Implicit.Higher.EffFunctor
import Control.Effect.Implicit.Higher.ContraLift

import qualified Control.Effect.Implicit.Base as Base
import qualified Control.Effect.Implicit.Freer as Base

class
  ( Base.EffOps ops
  , EffOps ops
  , Operation ops ~ UpperOps (Base.Operation ops)
  )
  => HigherOps ops where

class
  ( Base.EffCoOp ops
  , EffCoOp ops
  , CoOperation ops ~ UpperCoOp (Base.CoOperation ops)
  )
  => HigherCoOp ops where

data UpperOps ops
  (inEff :: Type -> Type)
  (eff :: Type -> Type)
  = UpperOps
    { innerOps' :: ops inEff
    , outerOps' :: ops eff
    }

data UpperCoOp coop (f :: Type -> Type) a =
  UpperOp (coop a)

instance
  ( Effect eff
  , Base.EffFunctor ops
  )
  => Base.EffFunctor (UpperOps ops eff)
  where
    effmap
      :: forall eff1 eff2
       . (Effect eff1, Effect eff2)
      => (forall x. eff1 x -> eff2 x)
      -> UpperOps ops eff eff1
      -> UpperOps ops eff eff2
    effmap lifter (UpperOps ops1 ops2) =
      UpperOps ops1 (Base.effmap lifter ops2)

instance
  (Base.EffFunctor ops)
  => EffFunctor (UpperOps ops)
   where
    invEffmap
      :: forall eff1 eff2
       . (Effect eff1, Effect eff2)
      => (forall x. eff1 x -> eff2 x)
      -> ContraLift eff1 eff2
      -> UpperOps ops eff1 eff1
      -> UpperOps ops eff2 eff2
    invEffmap lifter _ (UpperOps ops1 ops2) =
      UpperOps (Base.effmap lifter ops1) (Base.effmap lifter ops2)

instance
  (Functor coop)
  => Functor (UpperCoOp coop f)
  where
    fmap f (UpperOp ops) = UpperOp $ fmap f ops

instance
  (Functor coop)
  => CoOpFunctor (UpperCoOp coop)
  where
    liftCoOp _ (UpperOp ops) = (UpperOp ops)

instance
  {-# OVERLAPPABLE #-}
  ( HigherOps ops
  , HigherCoOp ops
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
           . UpperCoOp (Base.CoOperation ops) eff a
          -> eff a)
      -> UpperOps (Base.Operation ops) eff eff
    mkFreeOps liftCoOp1 = UpperOps ops ops
     where
      ops :: Base.Operation ops eff
      ops = Base.mkFreeOps liftCoOp2

      liftCoOp2
        :: forall a
         . Base.CoOperation ops a
        -> eff a
      liftCoOp2 op = liftCoOp1 $ UpperOp op

liftHigherOps
  :: forall ops eff
   . (Effect eff, HigherOps ops)
  => Base.Operation ops eff
  -> Operation ops eff eff
liftHigherOps ops = UpperOps ops ops

liftCoOpHandler
  :: forall ops eff f
   . (Effect eff, HigherOps ops, HigherCoOp ops)
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
       . UpperCoOp (Base.CoOperation ops) (eff ∘ f) a
      -> (a -> eff (f r))
      -> (eff (f r))
    handleOps (UpperOp coop) cont =
      Base.coOpHandler handler1 coop cont

lowerCoOpHandler
  :: forall ops eff f
   . (Effect eff, HigherOps ops, HigherCoOp ops)
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
      handleOp2 op = handleOp1 $ UpperOp op