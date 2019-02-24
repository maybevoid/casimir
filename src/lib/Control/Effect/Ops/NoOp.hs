
module Control.Effect.Ops.NoOp
  ( NoOp (..)
  , NoModel (..)
  )
where

import Control.Effect.Class
  ( EffFunctor (..)
  , FreeEff (..)
  , EffOps (..)
  )

data NoOp (eff :: * -> *) = NoOp

-- e.g. Proxy
data NoModel a = NoModel

instance Functor NoModel where
  fmap _ _ = NoModel

instance EffFunctor NoOp where
  effmap _ _ = NoOp

instance FreeEff NoOp where
  type FreeModel NoOp = NoModel

  freeModel _ = NoOp

instance EffOps NoOp where
  type EffConstraint NoOp eff = ()

  bindConstraint _ = id