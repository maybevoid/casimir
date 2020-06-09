{-# language PolyKinds #-}

module Casimir.Base.Effect
  ( Effect (..)
  , Effects (..)
  , Singleton
  , NoEff
  , Union
  , Cons
  )
where

import Data.Kind

import qualified QuasiParam.Casimir as Param

data NoEff
data Singleton eff
data Cons eff1 eff2
data Union eff1 eff2

class Effects (effs :: k) where
    type family Operations effs
      = (ops :: (Type -> Type) -> Type) | ops -> effs

class Effect eff where
  type family Operation eff
    = (ops :: (Type -> Type) -> Type) | ops -> eff

instance ( Effect eff )
  => Effects (Singleton eff) where
    type Operations (Singleton eff) =
      Param.Singleton (Operation eff)

instance Effects ('[] :: [Type]) where
  type Operations '[] = Param.Nil

instance
  ( Effects effs1
  , Effects effs2
  )
  => Effects (Union effs1 effs2) where
    type Operations (Union effs1 effs2) =
      Param.Union (Operations effs1) (Operations effs2)

instance
  ( Effect eff
  , Effects effs
  )
  => Effects (eff:effs) where
    type Operations (eff:effs) =
      Param.Cons (Operation eff) (Operations effs)
