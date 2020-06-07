{-# Language UndecidableInstances #-}

module Casimir.Higher.Free
where

import Data.Kind

import Casimir.Base
  (ContraLift (..), HigherLift (..))
import Casimir.Base.Implicit
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
  (eff :: Type -> Type)
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
  )
  => FreeOps ops
   where
    mkFreeOps
      :: forall eff
      . (Monad eff)
      => (forall a . CoOperation ops eff a -> eff a)
      -> Operation ops eff eff

class
  ( forall ops eff
     . (FreeOps ops, Monad eff)
    => Monad (free ops eff)
  )
  => FreeEff free
   where
    freeOps :: forall ops eff
       . (FreeOps ops, Monad eff)
      => Operation ops (free ops eff) (free ops eff)

    liftFree :: forall ops eff a
       . (FreeOps ops, Monad eff)
      => eff a
      -> free ops eff a

    freeContraLift
      :: forall eff ops
       . ( Monad eff
         , FreeOps ops
         )
      => ContraLift eff (free ops eff)


class
  (FreeEff free)
  => FreeHandler free
   where
    handleFree
      :: forall ops eff f a
       . ( Monad eff
         , FreeOps ops
         , Functor f
         )
      => CoOpHandler ops f eff
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
       . (Monad eff)
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

freeHigherLift
  :: forall free ops eff
   . (FreeEff free, FreeOps ops, Monad eff)
  => HigherLift eff (free ops eff)
freeHigherLift = HigherLift liftFree freeContraLift

{-# INLINE withCoOpHandler #-}
withCoOpHandler
  :: forall free ops eff f r
   . ( Functor f
     , Monad eff
     , FreeEff free
     , FreeHandler free
     , EffOps ops
     , FreeOps ops
     , ImplicitOps ops
     , LowerEffOps ops
     )
  => CoOpHandler ops f eff
  -> ((OpsConstraint ops (free ops eff))
      => free ops eff r)
  -> eff (f r)
withCoOpHandler handler comp1
  = handleFree @free handler $
      withOps ops1 comp1
 where
  ops1 :: Base.Operation ops (free ops eff)
  ops1 = LowerOps freeOps

liftCoOpHandler
  :: forall ops eff f
   . ( Monad eff
     , HigherEffOps ops
     , HigherEffCoOp ops
     )
  => (forall a . Base.CoOpHandler ops a (f a) eff)
  -> ContraFree eff f
  -> CoOpHandler ops f eff
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
   . ( Monad eff
     , HigherEffOps ops
     , HigherEffCoOp ops
     )
  => CoOpHandler ops f eff
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
