{-# Language UndecidableInstances #-}

module Casimir.Higher.Ops.UnderOps
where

import Casimir.Higher.Base

import Data.Kind
import Casimir.Base (EffFunctor (..))
import qualified Casimir.Base as Base

data UnderEff ops (inEff :: Type -> Type)

data UnderOps ops inEff eff where
  UnderOps
    :: forall ops inEff eff
     . (Effect eff, Effect inEff)
    => ops inEff eff
    -> UnderOps ops inEff eff

instance
  (EffFunctor lift (ops inEff))
  => EffFunctor lift (UnderOps ops inEff)
   where
    effmap lift (UnderOps ops) =
      UnderOps $ effmap lift ops

instance
  (EffOps ops)
  => Base.EffOps (UnderEff ops eff) where
    type Operation (UnderEff ops eff) =
      UnderOps (Operation ops) eff
