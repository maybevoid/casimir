
module Control.Effect.Implicit.Higher.Ops.Exception
where

import Data.Void

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.EffFunctor

data ExceptionOps e inEff eff = ExceptionOps
  { tryOp
      :: forall a
      . inEff a
      -> (e -> eff a)
      -> eff a

  , throwOp :: e -> inEff Void
  }

instance
  (Effect inEff)
  => EffFunctor (ExceptionOps e inEff) where
    effmap _ = undefined

instance HEffFunctor (ExceptionOps e) where
  invEffmap _ = undefined
  contraEffmap _ = undefined
