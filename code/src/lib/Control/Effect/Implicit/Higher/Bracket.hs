
module Control.Effect.Implicit.Higher.Bracket
where

import Control.Exception (bracket)

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.HigherOps
import Control.Effect.Implicit.Higher.Weaver

data BracketEff

data BracketOps inEff eff = BracketOps {
  bracketOp
    :: forall a b
     . IO a
    -> (a -> IO ())
    -> (a -> inEff b)
    -> eff b
}

instance HigherOps BracketEff where
  type HOperation BracketEff = BracketOps

instance HigherEffFunctor BracketOps where
  liftHigherOps
    :: forall eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => (forall x . eff1 x -> eff2 x)
    -> Weaver eff1 eff2
    -> BracketOps eff1 eff1
    -> BracketOps eff2 eff2
  liftHigherOps lifter (Weaver weaver) (BracketOps doBracket) = BracketOps ops
   where
    ops
      :: forall a b
       . IO a
      -> (a -> IO ())
      -> (a -> eff2 b)
      -> eff2 b
    ops alloc release cont1 = weaver >>= cont2
     where
      cont2
        :: forall w
         . WeaverOps eff1 eff2 w
        -> eff2 b
      cont2 weaver2 = do
        mx <- cont3
        weaveOut weaver2 mx
       where
        cont3 :: eff2 (w b)
        cont3 = lifter $ doBracket alloc release cont4

        cont4 :: a -> eff1 (w b)
        cont4 x = weaveIn weaver2 $ cont1 x

ioBracketOps :: BracketOps IO IO
ioBracketOps = BracketOps bracket
