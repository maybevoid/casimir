
module Control.Effect.Implicit.Freer.FreerEff
  ( FreerEff (..)
  , FreerCoOpHandler (..)
  , FreerEffCoOp (..)
  , FreerOps (..)
  , CoOpCont (..)
  )
where

import Data.Kind

import Control.Effect.Implicit.Base

class FreerEffCoOp sig where
  type family FreerCoOp sig
    = (coop :: (Type -> Type)) | coop -> sig

data CoOpCont ops a where
  CoOpCont
    :: forall ops a x
      . FreerCoOp ops x -> (x -> a) -> CoOpCont ops a

class
  ( EffOps ops
  , FreerEffCoOp ops
  , Functor (CoOperation ops)
  , EffFunctor (Operation ops)
  )
  => FreerOps (ops :: Type) where
    mkFreerOps
      :: forall t eff
       . ( Effect eff
         , Effect (t eff)
         )
      => (forall a . FreerCoOp ops a -> t eff a)
      -> Operation ops (t eff)

data FreerCoOpHandler handler a r eff =
  FreerCoOpHandler {
    handleFreerReturn :: a -> eff r,
    handleFreerCoOp :: CoOpCont handler (eff r) -> eff r
  }

class
  (forall ops eff . (FreerOps ops, Effect eff) => Monad (free ops eff))
  => FreerEff free where
    freerOps :: forall ops eff
      . (FreerOps ops, Effect eff)
      => Operation ops (free ops eff)

    liftFreer :: forall ops eff a
      . (FreeOps ops, Effect eff)
      => eff a -> free ops eff a

    handleFreer
      :: forall ops eff a r
      . (Effect eff, FreerOps ops)
      => FreerCoOpHandler ops a r eff
      -> free ops eff a
      -> eff r
