
module Control.Effect.Implicit.Higher.Weaver
where

data WeaverOps eff1 eff2 w = WeaverOps
  { weaveIn :: forall x . eff2 x -> eff1 (w x)
  , weaveOut :: forall x . w x -> eff2 x
  }

data Weaver eff1 eff2 where
  Weaver :: forall eff1 eff2 w . eff2 (WeaverOps eff1 eff2 w) -> Weaver eff1 eff2
