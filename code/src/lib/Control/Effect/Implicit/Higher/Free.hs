module Control.Effect.Implicit.Higher.Free

where

import Data.Kind
import Control.Monad.Identity

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.EffFunctor
import Control.Effect.Implicit.Higher.ContraLift

newtype Nest w eff a = Nest (eff (w a))
newtype NoContext eff a = NoContext (eff a)

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
     . (Monad f2)
    => (forall x . f1 x -> f2 x)
    -> ContraLiftEff' f1 f2
    -> CoOpHandler ops f1
    -> CoOpHandler ops f2

-- class CoOpFunctor coop where
--   mapInner
--     :: forall f w1 w2 a
--       . (forall x . w1 x -> w2 x)
--     -> (coop f w1 a)
--     -> (coop f w2 a)

--   mapOuter
--     :: forall f1 f2 w a
--       . (forall x . f1 x -> f2 x)
--     -> (coop f1 w a)
--     -> (coop f2 w a)

-- mapCoOpHandler
--   :: forall ops f1 f2 w1 w2
--    . (EffCoOp ops)
--   => (forall x . CoOperation ops f1 f1 x -> CoOperation ops f2 w2 x)
--   -> CoOpHandler ops f1 w1
--   -> CoOpHandler ops f2 w2
-- mapCoOpHandler liftCoOp = undefined

-- data Weaver
--   (f :: Type -> Type)
--   (w :: Type -> Type)
--   = Weaver
--     { weaverHandler
--         :: forall a b
--          . ((f a -> w a)
--             -> w a)
--         -> (a -> f b)
--         -> f b
--     }

-- data Finalizer
--   (f :: (Type -> Type) -> Type -> Type)
--   (w :: Type -> Type)
--   (eff :: Type -> Type)
--   = Finalizer
--     { finalizerHandler
--         :: forall a
--         . w ()
--         -> f eff a
--         -> eff (w a)
--     }

-- class
--   ( EffCoOp ops
--   , CoOpFunctor (CoOperation ops)
--   , HigherEffFunctor ops
--   )
--   => FreeOps ops where
--     mkFreeOps
--       :: forall eff
--        . (Effect eff)
--       => (forall a . CoOperation ops Identity eff a -> eff a)
--       -> ops eff eff

-- class
--   ( forall ops eff
--     . (FreeOps ops, Effect eff)
--     => Monad (free ops eff)
--   )
--   => FreeEff free
--    where
--     freeOps :: forall ops eff .
--       (FreeOps ops, Effect eff)
--       => ops (free ops eff) (free ops eff)

--     liftFree :: forall ops eff a .
--       (FreeOps ops, Effect eff)
--       => eff a
--       -> free ops eff a

-- class
--   (FreeEff free)
--   => FreeHandler free
--    where
--     handleFree
--       :: forall ops f w eff a
--        . ( Effect eff
--          , FreeOps ops
--          , Functor (f eff)
--          , Functor w
--          )
--       => CoOpHandler ops (f eff) w
--       -> free ops eff a
--       -> f eff a

--     handleScoped
--       :: forall ops f w eff a
--        . ( Effect eff
--          , FreeOps ops
--          , Functor (f eff)
--          , Functor w
--          )
--       => CoOpHandler ops (f eff) (Nest w eff)
--       -> Finalizer f w eff
--       -> CoOperation ops Identity (free ops eff) a
--       -> CoOperation ops (f eff) (Nest w eff) a

--     handleContraLift
--       :: forall ops f w eff
--        . ( Effect eff
--          , FreeOps ops
--          , Functor (f eff)
--          , Functor w
--          )
--       => CoOpHandler ops (f eff) (Nest w eff)
--       -> Finalizer f w eff
--       -> Weaver (f eff) (Nest w eff)
--       -> ContraLiftEff eff (free ops eff)