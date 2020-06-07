{-# Language UndecidableInstances #-}

module Casimir.Higher.Ops.UnderOps
where

import Casimir.Higher.Base

import Data.Kind
import Casimir.Base (EffFunctor (..))
import qualified Casimir.Base as Base

data UnderEff ops (inEff :: Type -> Type)

data UnderOps ops inEff m where
  UnderOps
    :: forall ops inEff m
     . (Monad m, Monad inEff)
    => ops inEff m
    -> UnderOps ops inEff m

instance
  (EffFunctor lift (ops inEff))
  => EffFunctor lift (UnderOps ops inEff)
   where
    effmap lift (UnderOps ops) =
      UnderOps $ effmap lift ops

instance
  (Effect ops)
  => Base.Effect (UnderEff ops m) where
    type Operation (UnderEff ops m) =
      UnderOps (Operation ops) m
