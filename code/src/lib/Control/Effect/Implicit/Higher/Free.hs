module Control.Effect.Implicit.Higher.Free

where

import Data.Kind

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.EffFunctor
import Control.Effect.Implicit.Higher.ContraLift

newtype Nest f a = Nest (f (f a))

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

    , joinHandler
        :: forall f a
         . f (f a)
        -> f a

    , coOpHandler
        :: forall a r
        . CoOperation ops f a
        -> (a -> f (f r))
        -> f r
    }

class
  ( EffCoOp ops
  )
  => CoOpFunctor ops
  where
    mapCoOp
      :: forall f a b
       . (Functor f)
      => (a -> b)
      -> CoOperation ops f a
      -> CoOperation ops f b

    liftCoOp
      :: forall f1 f2 a
       . (Functor f1, Functor f2)
      => (forall x . f1 x -> f2 x)
      -> CoOperation ops f1 a
      -> CoOperation ops f2 a

class
  ( EffCoOp ops
  , CoOpFunctor ops
  , HigherEffFunctor ops
  )
  => FreeOps ops where
    mkFreeOps
      :: forall f eff
      . (Effect eff)
      => (forall a . CoOperation ops f a -> eff a)
      -> ops eff eff

class
  ( forall ops eff
    . (FreeOps ops, Effect eff)
    => Monad (free ops eff)
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
    handleFree
      :: forall ops eff t a r
       . ( Effect eff
         , FreeOps ops
         , Functor (t eff)
         )
      => CoOpHandler ops (t eff)
      -> free ops eff a
      -> (a -> t eff r)
      -> t eff r

class
  (FreeEff free)
  => HigherFreeHandler free
   where
    handleContraLift
      :: forall eff ops
       . ( Effect eff
         , FreeOps ops
         )
      => ContraLift eff (free ops eff)

    handleHigherFree
      :: forall ops eff t a r
       . ( Effect eff
         , FreeOps ops
         , Functor (t eff)
         )
      => CoOpHandler ops (t eff)
      -> ContraLift eff (t eff)
      -> free ops eff a
      -> (a -> t eff r)
      -> t eff r

-- class CoOpFunctor ops where
--   mapCoOpHandler
--     :: forall f1 f2
--      . Monad f2
--     => (forall x . f1 x -> f2 x)
--     -> ContraLift f1 f2
--     -> CoOpHandler ops f1
--     -> CoOpHandler ops f2

-- freeContraLift
--   :: forall t w ops eff free
--    . ( Functor w
--      , EffCoOp ops
--      , Effect eff
--      , FreeOps ops
--      , FreeEff free
--      )
--   => CoOpHandler ops (t eff)
--   -> (((forall a . t eff a -> eff (w a))
--        -> eff (w a))
--       -> t eff a)
--   -> ContraLift w (free ops eff) eff
-- freeContraLift _ _ = undefined
  -- weaver suspend =
  -- ContraLift contraLift1
  --  where
  --   contraLift1
  --     :: forall a
  --     . ((forall x . eff2 x -> eff1 (w x))
  --         -> eff1 (w a))
  --     -> eff2 a
  --   contraLift1 cont = do
  --     context1 <- suspend
  --     undefined