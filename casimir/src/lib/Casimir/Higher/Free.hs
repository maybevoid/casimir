{-# Language UndecidableInstances #-}

module Casimir.Higher.Free
where

import Data.Kind

import Casimir.Base
  ( ContraLift (..)
  , HigherLift (..)
  , ImplicitOps (..)
  , OpsConstraint
  )

import Casimir.Higher.Base
import Casimir.Higher.CoOp
import Casimir.Higher.ContraLift

import qualified Casimir.Base as Base
import qualified Casimir.Freer as Base

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
  ( EffCoOp ops
  , CoOpFunctor (CoOperation ops)
  )
  => FreeOps ops
   where
    mkFreeOps
      :: forall m
      . (Monad m)
      => (forall a . CoOperation ops m a -> m a)
      -> Operation ops m m

class
  ( forall ops m
     . (FreeOps ops, Monad m)
    => Monad (free ops m)
  )
  => FreeEff free
   where
    freeOps :: forall ops m
       . (FreeOps ops, Monad m)
      => Operation ops (free ops m) (free ops m)

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
  ( HigherEffect ops
  , HigherEffCoOp ops
  , EffCoOp ops
  , Functor (Base.CoOperation ops)
  , Base.FreeOps ops
  )
  => FreeOps ops
  where
    mkFreeOps
      :: forall m
       . (Monad m)
      => (forall a
           . HigherCoOp (Base.CoOperation ops) m a
          -> m a)
      -> HigherOps (Base.Operation ops) m m
    mkFreeOps liftCoOp1 = HigherOps ops
     where
      ops :: Base.Operation ops m
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
  :: forall free ops m f r
   . ( Functor f
     , Monad m
     , FreeEff free
     , FreeHandler free
     , Effect ops
     , FreeOps ops
     , ImplicitOps ops
     , LowerEffect ops
     )
  => CoOpHandler ops f m
  -> ((OpsConstraint ops (free ops m))
      => free ops m r)
  -> m (f r)
withCoOpHandler handler comp1
  = handleFree @free handler $
      withOps ops1 comp1
 where
  ops1 :: Base.Operation ops (free ops m)
  ops1 = LowerOps freeOps

liftCoOpHandler
  :: forall ops m f
   . ( Monad m
     , HigherEffect ops
     , HigherEffCoOp ops
     )
  => (forall a . Base.CoOpHandler ops a (f a) m)
  -> ContraFree m f
  -> CoOpHandler ops f m
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
  :: forall ops m f
   . ( Monad m
     , HigherEffect ops
     , HigherEffCoOp ops
     )
  => CoOpHandler ops f m
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
