
module Control.Effect.Implicit.Higher.Ops.Bracket
where

import Control.Exception (bracket)

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.EffFunctor
import Control.Effect.Implicit.Higher.ContraLift
import Control.Effect.Implicit.Higher.Free

data BracketOps inEff eff = BracketOps {
  bracketOp
    :: forall a b
     . IO a
    -> (a -> IO ())
    -> (a -> inEff b)
    -> eff b
}

data BracketCoOp f a where
  BracketOp
    :: forall f a b c
     . IO a
    -> (a -> IO ())
    -> (a -> f b)
    -> (b -> f c)
    -> BracketCoOp f c

instance
  (Effect inEff)
  => EffFunctor (BracketOps inEff) where
    effmap _ = undefined

instance HigherEffFunctor BracketOps where
  invEffmap
    :: forall w eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => (forall x . eff1 x -> eff2 x)
    -> ContraLift w eff1 eff2
    -> BracketOps eff1 eff1
    -> BracketOps eff2 eff2
  invEffmap _
    (ContraLift contraLift1)
    (BracketOps doBracket) =
      BracketOps ops
       where
        ops
          :: forall a b
            . IO a
          -> (a -> IO ())
          -> (a -> eff2 b)
          -> eff2 b
        ops alloc release cont1 = contraLift1 cont2
         where
          cont2
            :: (forall x . eff2 x -> eff1 (w x))
            -> eff1 (w b)
          cont2 contraLift2 = doBracket alloc release cont3
           where
            cont3 :: a -> eff1 (w b)
            cont3 x = contraLift2 $ cont1 x

  contraEffmap _ = undefined

ioBracketOps :: BracketOps IO IO
ioBracketOps = BracketOps bracket

instance EffCoOp BracketOps where
  type CoOperation BracketOps = BracketCoOp

instance
  (Functor f)
  => Functor (BracketCoOp f) where
    fmap mapper (BracketOp alloc release comp cont1) =
      BracketOp alloc release comp cont2
       where
        cont2 = fmap (fmap mapper) cont1

bracketCoOpHandler
  :: CoOpHandler BracketOps IO
bracketCoOpHandler = CoOpHandler return handleOp
 where
  handleOp (BracketOp alloc release comp cont) =
    bracket alloc release comp >>= cont

instance CoOpFunctor BracketOps where
  mapCoOpHandler
    :: forall w f1 f2
     . (Monad f2)
    => (forall x . f1 x -> f2 x)
    -> ContraLift w f1 f2
    -> CoOpHandler BracketOps f1
    -> CoOpHandler BracketOps f2
  mapCoOpHandler
    lift
    (ContraLift contraLift1)
    (CoOpHandler handleReturn1 handleOp1) =
    CoOpHandler handleReturn2 handleOp2
     where
      handleReturn2
        :: forall a . a -> f2 a
      handleReturn2 = lift . handleReturn1

      handleOp2
        :: forall a
         . BracketCoOp f2 a
        -> f2 a
      handleOp2
        (BracketOp alloc1 release1 comp1 cont1)
        = handleOp3 alloc1 release1 comp1 cont1
         where
          handleOp3
            :: forall b c
             . IO b
            -> (b -> IO ())
            -> (b -> f2 c)
            -> (c -> f2 a)
            -> f2 a
          handleOp3 alloc2 release2 comp2 cont2 =
            res1 >>= cont2
             where
              res1 :: f2 c
              res1 = contraLift1 cont3

              cont3
                :: (forall x . f2 x -> f1 (w x))
                -> f1 (w c)
              cont3 contraLift2 = handleOp1 $
                BracketOp alloc2 release2 comp3 handleReturn1
               where
                comp3 :: (b -> f1 (w c))
                comp3 = contraLift2 . comp2
