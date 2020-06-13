{-# Language UndecidableInstances #-}

module Casimir.Higher.Free
where

import Data.Kind

import Casimir.Base
  ( type (~>)
  , ContraLift (..)
  , HigherLift (..)
  , ImplicitOps
  , OpsConstraint
  , withOps
  )

import Casimir.Higher.Base
import Casimir.Higher.ContraLift

import qualified Casimir.Base as Base
import qualified Casimir.Freer as Base

newtype Nest f g a = Nest
  { unNest :: f (g a) }

type f ∘ g = Nest f g

data HigherCoOp coop (f :: Type -> Type) a =
  HigherOp (coop a)

class CoOpFunctor coop where
  liftCoOp
    :: forall f1 f2 a
     . (Functor f1, Functor f2)
    => f1 ~> f2
    -> coop f1 a
    -> coop f2 a

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

instance
  (Functor f, Functor g)
  => Functor (Nest f g) where
    fmap f (Nest mx) = Nest $ fmap (fmap f) mx

data CoOpHandler
  ops
  (f :: Type -> Type)
  (m :: Type -> Type)
  = CoOpHandler
      { returnHandler
          :: forall a . a -> m (f a)

      , operationHandler
          :: forall a r
          . CoOperation ops (m ∘ f) a
          -> (a -> m (f r))
          -> m (f r)

      , contraLiftHandler
          :: ContraFree m f
      }

class
  ( CoOpFunctor (CoOperation ops) )
  => FreeOps ops where
  type family CoOperation ops
    :: (Type -> Type)
    -> Type
    -> Type

  mkFreeOps
    :: forall m
    . (Monad m)
    => (forall a . CoOperation ops m a -> m a)
    -> ops m m

class
  ( forall ops m
     . (FreeOps ops, Monad m)
    => Monad (free ops m)
  )
  => FreeEff free
   where
    freeOps :: forall ops m
       . (FreeOps ops, Monad m)
      => ops (free ops m) (free ops m)

    liftFree :: forall ops m a
       . (FreeOps ops, Monad m)
      => m a
      -> free ops m a

    freeContraLift
      :: forall m ops
       . ( Monad m
         , FreeOps ops
         )
      => ContraLift m (free ops m)

class
  (FreeEff free)
  => FreeHandler free
   where
    handleFree
      :: forall ops m f a
       . ( Monad m
         , FreeOps ops
         , Functor f
         )
      => CoOpHandler ops f m
      -> free ops m a
      -> m (f a)

instance
  {-# OVERLAPPABLE #-}
  ( Base.FreeOps ops
  , Functor (Base.CoOperation ops)
  )
  => FreeOps (HigherOps ops) where
    type CoOperation (HigherOps ops) =
      HigherCoOp (Base.CoOperation ops)

    mkFreeOps
      :: forall m
       . (Monad m)
      => (forall a
           . HigherCoOp (Base.CoOperation ops) m a
          -> m a)
      -> HigherOps ops m m
    mkFreeOps liftCoOp1 = HigherOps ops
     where
      ops :: ops m
      ops = Base.mkFreeOps liftCoOp2

      liftCoOp2
        :: forall a
         . Base.CoOperation ops a
        -> m a
      liftCoOp2 op = liftCoOp1 $ HigherOp op

freeHigherLift
  :: forall free ops m
   . (FreeEff free, FreeOps ops, Monad m)
  => HigherLift m (free ops m)
freeHigherLift = HigherLift liftFree freeContraLift

{-# INLINE withCoOpHandler #-}
withCoOpHandler
  :: forall free eff ops m f r
   . ( Functor f
     , Monad m
     , Effects eff
     , Base.Effects eff
     , FreeOps ops
     , FreeEff free
     , ImplicitOps eff
     , FreeHandler free
     , Operations eff ~ ops
     , Base.Operations eff ~ LowerOps ops
     )
  => CoOpHandler ops f m
  -> ((OpsConstraint eff (free ops m))
      => free ops m r)
  -> m (f r)
withCoOpHandler handler comp1
  = handleFree @free handler $
      withOps ops1 comp1
 where
  ops1 :: LowerOps ops (free ops m)
  ops1 = LowerOps freeOps

liftCoOpHandler
  :: forall eff ops m f
   . ( Monad m
     , Effects eff
     , Base.Effects eff
     , Base.Operations eff ~ ops
     , Operations eff ~ HigherOps ops
     )
  => (forall a . Base.CoOpHandler ops a (f a) m)
  -> ContraFree m f
  -> CoOpHandler (HigherOps ops) f m
liftCoOpHandler handler1 contraLift =
  CoOpHandler handleReturn handleOps contraLift
   where
    handleReturn :: forall a . a -> m (f a)
    handleReturn = Base.returnHandler handler1

    handleOps
      :: forall a r
       . HigherCoOp (Base.CoOperation ops) (m ∘ f) a
      -> (a -> m (f r))
      -> (m (f r))
    handleOps (HigherOp coop) cont =
      Base.coOpHandler handler1 coop cont

lowerCoOpHandler
  :: forall eff ops m f
   . ( Monad m
     , Effects eff
     , Base.Effects eff
     , Base.Operations eff ~ ops
     , Operations eff ~ HigherOps ops
     )
  => CoOpHandler (HigherOps ops) f m
  -> (forall a . Base.CoOpHandler ops a (f a) m)
lowerCoOpHandler
  (CoOpHandler handleReturn1 handleOp1 _) =
  handler2
   where
    handler2 :: forall a . Base.CoOpHandler ops a (f a) m
    handler2 = Base.CoOpHandler handleReturn1 handleOp2
     where
      handleOp2
        :: forall x
         . Base.CoOperation ops x
        -> (x -> m (f a))
        -> m (f a)
      handleOp2 op = handleOp1 $ HigherOp op
