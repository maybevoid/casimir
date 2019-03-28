
module Control.Effect.Implicit.Computation.Class
  ( Computation (..)
  , Handler
  )
where

import Control.Effect.Implicit.Base

newtype Computation ops comp eff1 = Computation {
  runComp :: forall eff2 .
    ( EffOps ops
    , Effect eff1
    , Effect eff2
    )
    => LiftEff eff1 eff2
    -> Operation ops eff2
    -> comp eff2
}

type Handler ops handler eff
  = Computation ops (Operation handler) eff
