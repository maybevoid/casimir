module Control.Effect.Implicit.Higher.Implicit
where

import Data.Kind
import Control.Effect.Implicit.Higher.Base
import qualified Control.Effect.Implicit.Base as Base

class
  (EffOps ops)
  => ImplicitOps ops where
    type family OpsConstraint ops (eff1 :: Type -> Type) (eff2 :: Type -> Type)
      = (c :: Constraint) | c -> ops eff1 eff2

    withHigherOps
      :: forall eff1 eff2 r
       . (Effect eff1, Effect eff2)
      => Operation ops eff1 eff2
      -> (OpsConstraint ops eff1 eff2 => r)
      -> r

    captureHigherOps
      :: forall eff1 eff2
       . ( Effect eff1
         , Effect eff2
         , OpsConstraint ops eff1 eff2
         )
      => Operation ops eff1 eff2

type EffConstraint ops eff1 eff2 =
  ( Effect eff1
  , Effect eff2
  , ImplicitOps ops
  , OpsConstraint ops eff1 eff2
  )

type Eff lowerOps higherOps a =
  forall eff
   . ( Effect eff
     , Base.ImplicitOps lowerOps
     , ImplicitOps higherOps
     , Base.OpsConstraint lowerOps eff
     , OpsConstraint higherOps eff eff
     )
  => eff a

withOps
  :: forall lowerOps higherOps eff r
   . ( Effect eff
     , Base.ImplicitOps lowerOps
     , ImplicitOps higherOps
     )
  => Base.Operation lowerOps eff
  -> Operation higherOps eff eff
  -> (( Base.OpsConstraint lowerOps eff
      , OpsConstraint higherOps eff eff
      ) => r)
  -> r
withOps lowerOps higherOps cont =
  Base.withOps lowerOps $
    withHigherOps higherOps cont
