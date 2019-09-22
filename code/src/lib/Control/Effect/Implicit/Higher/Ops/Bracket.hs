
module Control.Effect.Implicit.Higher.Ops.Bracket
where

import Control.Exception (bracket)

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.EffFunctor
import Control.Effect.Implicit.Higher.ContraLift

data BracketOps inEff eff = BracketOps {
  bracketOp
    :: forall a b
     . IO a
    -> (a -> IO ())
    -> (a -> inEff b)
    -> eff b
}

instance
  (Effect inEff)
  => EffFunctor (BracketOps inEff) where
    effmap _ = undefined

instance HigherEffFunctor BracketOps where
  invEffmap
    :: forall eff1 eff2
     . ( Effect eff1
       , Effect eff2
       )
    => (forall x . eff1 x -> eff2 x)
    -> ContraLiftEff eff1 eff2
    -> BracketOps eff1 eff1
    -> BracketOps eff2 eff2
  invEffmap lifter contraLift1 (BracketOps doBracket) =
    BracketOps ops
     where
      ops
        :: forall a b
         . IO a
        -> (a -> IO ())
        -> (a -> eff2 b)
        -> eff2 b
      ops alloc release cont1 = withContraLift contraLift1 cont2
       where
        cont2
          :: forall w
          . ContraLiftOps eff1 eff2 w
          -> eff2 b
        cont2 contraLift2 = do
          mx <- cont3
          liftResume contraLift2 mx
           where
            cont3 :: eff2 (w b)
            cont3 = lifter $ doBracket alloc release cont4

            cont4 :: a -> eff1 (w b)
            cont4 x = contraLiftEff contraLift2 $ cont1 x

  contraEffmap _ = undefined

-- bracket
--   :: forall inEff eff a b
--    . (Effect inEff, Effect eff)
--   => IO a
--   -> (a -> IO ())
--   -> (a -> inEff b)
--   -> eff b
-- bracket

ioBracketOps :: BracketOps IO IO
ioBracketOps = BracketOps bracket
