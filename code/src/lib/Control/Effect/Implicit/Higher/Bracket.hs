
module Control.Effect.Implicit.Higher.Bracket
where

import Control.Exception (bracket)

import Control.Effect.Implicit.Higher.HigherOps

data BracketEff

data BracketOps inEff eff = BracketOps {
  bracketOp
    :: forall a b
     . IO a
    -> (a -> IO ())
    -> (a -> inEff b)
    -> eff b
}

instance HigherOps BracketEff
   where
    type HOperation BracketEff  =
      BracketOps

instance HigherOpsFunctor BracketOps where
    liftHigherOps ops weaver = BracketOps undefined

ioBracketOps :: BracketOps IO IO
ioBracketOps = BracketOps bracket
