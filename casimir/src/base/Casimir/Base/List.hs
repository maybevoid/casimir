module Casimir.Base.List
  ( Cons
  , EffectList (..)
  , type (:+)
  , pattern Cons
  )
where

import Data.Kind
import Casimir.Base.Effect
import Casimir.Base.Union
import Casimir.Base.NoOp
import qualified QuasiParam.Casimir as Param

type Cons eff1 eff2 = Union (Singleton eff1) eff2

type ConsOps ops1 ops2 = Param.Cons ops1 ops2

infixr 6 :+
type ops1 :+ ops2 = Cons ops1 ops2

pattern Cons
  :: forall ops1 ops2 m
   . ops1 m
  -> ops2 m
  -> ConsOps ops1 ops2 m
pattern Cons ops1 ops2 = Param.Cons ops1 ops2

class
  ( Effects (ToEffects xs) )
  => EffectList (xs :: [Type]) where
    type family ToEffects xs = eff | eff -> xs

instance
  ( Effect x
  , EffectList xs
  )
  => EffectList (x:xs) where
    type ToEffects (x:xs) = Cons x (ToEffects xs)

instance EffectList '[] where
  type ToEffects '[] = NoEff
