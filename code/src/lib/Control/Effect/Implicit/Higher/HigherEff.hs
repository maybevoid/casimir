
module Control.Effect.Implicit.Higher.HigherEff
where

import Data.Kind

import Control.Effect.Implicit.Base

data WeaverOps eff1 eff2 w = WeaverOps
  { weaveIn :: forall x . eff1 x -> eff2 (w x)
  , weaveOut :: forall x . w x -> eff1 x
  }

newtype Weaver eff1 eff2 = Weaver {
  weaverOps :: forall w . WeaverOps eff1 eff2 w
}

class
  (forall inOps
    . (EffOps inOps)
   => EffOps (LowerEff ops inOps))
  => HigherEff ops where
    type family LowerEff ops inOps

    type family HOperation ops (eff :: Type -> Type)
      :: (Type -> Type) -> Type

    type family TransformEff ops (eff :: Type -> Type)
      :: Type -> Type

    higherOps
      :: forall eff
       . (Effect eff)
      => HOperation ops
          (TransformEff ops eff)
          (TransformEff ops eff)

    liftHigherOps
      :: forall eff ops2
       . ( Effect eff
         , HigherEff ops2
         )
      => HOperation ops eff eff
      -> HOperation ops (TransformEff ops2 eff) (TransformEff ops2 eff)

    packHigherOps
      :: forall eff1 eff2 inOps
       . ( Effect eff1
         , Effect eff2
         , ImplicitOps inOps
         )
      => Operation inOps eff1
      -> HOperation ops eff1 eff2
      -> Operation (LowerEff ops inOps) eff2

    transformEff
      :: forall eff a
      . (Effect eff)
      => eff a
      -> TransformEff ops eff a

    liftWeaver
      :: forall eff1 eff2
      . (Effect eff1, Effect eff2)
      => (Weaver eff1 eff2)
      -> (TransformEff ops eff1
            (Weaver
              (TransformEff ops eff1)
              eff2))
