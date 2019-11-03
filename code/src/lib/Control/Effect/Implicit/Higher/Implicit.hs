module Control.Effect.Implicit.Higher.Implicit
where

import Data.Kind
import Control.Effect.Implicit.Higher.Base

class
  (EffOps ops)
  => ImplicitOps ops where
    type family OpsConstraint ops (eff1 :: Type -> Type) (eff2 :: Type -> Type)
      = (c :: Constraint) | c -> ops eff1 eff2

    withOps
      :: forall eff1 eff2 r
       . (Effect eff1, Effect eff2)
      => Operation ops eff1 eff2
      -> (OpsConstraint ops eff1 eff2 => r)
      -> r

    captureOps
      :: forall eff1 eff2
       . ( Effect eff1
         , Effect eff2
         , OpsConstraint ops eff1 eff2
         )
      => Operation ops eff1 eff2
