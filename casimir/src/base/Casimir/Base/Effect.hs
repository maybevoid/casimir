{-# language PolyKinds #-}

module Casimir.Base.Effect
  ( Effect (..)
  , Effects (..)
  , NormalizedEffects (..)
  , Operations
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

class NormalizedEffects eff where
  type family Operations' eff
    = (ops :: (Type -> Type) -> Type) | ops -> eff

class
  ( NormalizedEffects (NormalizeEffect xs) )
  => Effects (xs :: k) where
    type family NormalizeEffect xs = eff | eff -> xs

class Effect eff where
  type family Operation eff
    = (ops :: (Type -> Type) -> Type) | ops -> eff

type Operations effs = Operations' (NormalizeEffect effs)

instance ( Effect eff )
  => Effects (Singleton eff) where
    type NormalizeEffect (Singleton eff) = Singleton eff

instance
  ( Effect eff )
  => NormalizedEffects (Singleton eff) where
    type Operations' (Singleton eff) =
      Param.Singleton (Operation eff)


instance Effects ('[] :: [Type]) where
  type NormalizeEffect '[] = NoEff

instance NormalizedEffects NoEff where
  type Operations' NoEff = Param.Nil


instance
  (NormalizedEffects ops1, NormalizedEffects ops2)
  => NormalizedEffects (Union ops1 ops2)
  where
    type Operations' (Union ops1 ops2) =
      Param.Union (Operations' ops1) (Operations' ops2)

instance
  ( Effects effs1
  , Effects effs2
  )
  => Effects (Union effs1 effs2) where
    type NormalizeEffect (Union effs1 effs2) =
      Union (NormalizeEffect effs1) (NormalizeEffect effs2)


instance
  ( Effect x
  , Effects xs
  )
  => Effects (x:xs) where
    type NormalizeEffect (x:xs) =
      Cons x (NormalizeEffect xs)

instance
  ( Effect eff
  , NormalizedEffects effs
  )
  => NormalizedEffects (Cons eff effs) where
    type Operations' (Cons eff effs) =
      Param.Cons (Operation eff) (Operations' effs)
