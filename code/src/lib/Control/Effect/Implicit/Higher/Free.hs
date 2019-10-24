module Control.Effect.Implicit.Higher.Free

where

import Data.Kind

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.EffFunctor
import Control.Effect.Implicit.Higher.ContraLift

newtype Nest eff f a = Nest (eff (f a))

data ContraFree eff f = ContraFree {
  runContraFree
    :: forall a b
     . (forall w
         . (Functor w)
        => (forall x . f x -> eff (w x))
        -> eff (w a))
    -> (a -> eff (f b))
    -> eff (f b)
}

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
        . CoOperation ops (Nest eff f) a
        -> (a -> eff (f r))
        -> eff (f r)
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
  , HigherEffFunctor ops
  )
  => FreeOps ops where
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
      -> ContraFree eff f
      -> free ops eff a
      -> eff (f a)

data CoState s eff a = CoState {
  runCoState :: s -> eff (s, a)
}

contraState
  :: forall s eff
   . (Effect eff)
  => ContraFree eff (CoState s eff)
contraState = ContraFree handler1
 where
  handler1
    :: forall a b
     . ((forall x . CoState s eff x -> eff (s, x))
        -> eff (s, a))
    -> (a -> eff (CoState s eff b))
    -> eff (CoState s eff b)
  handler1 cont1 cont2 = return $ CoState handler2
   where
    handler2 :: s -> eff (s, b)
    handler2 s1 = do
      (s2, x) <- cont1 $ flip runCoState s1
      cont3 <- cont2 x
      runCoState cont3 s2

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