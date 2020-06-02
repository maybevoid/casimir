
module Casimir.Base.NoOp
  ( NoEff
  , NoOp (..)
  , pattern NoOp
  )
where

import Data.Kind
import Casimir.Base.Lift
import Casimir.Base.EffOps
import Casimir.Base.EffFunctor

data NoEff

newtype NoOp (m :: Type -> Type) = MkNoOp ()

pattern NoOp = MkNoOp ()

instance EffOps NoEff where
  type Operation NoEff = NoOp

instance EffFunctor lift NoOp where
  effmap _ _ = NoOp
