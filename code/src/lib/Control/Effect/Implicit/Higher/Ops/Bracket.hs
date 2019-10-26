
module Control.Effect.Implicit.Higher.Ops.Bracket
where

import Control.Monad.Identity
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

data BracketCoOp f r where
  BracketOp
    :: forall f a r
     . IO a
    -> (a -> IO ())
    -> (a -> f r)
    -> BracketCoOp f r

instance
  (Effect inEff)
  => EffFunctor (BracketOps inEff) where
    effmap _ = undefined

instance HEffFunctor BracketOps where
  invEffmap
    :: forall eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => (forall x . eff1 x -> eff2 x)
    -> ContraLift eff1 eff2
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
            :: forall w
             . (forall x . eff2 x -> eff1 (w x))
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
    fmap f (BracketOp alloc release comp) =
      BracketOp alloc release (fmap (fmap f) comp)

instance CoOpFunctor BracketOps where
  liftCoOp
    :: forall f1 f2 a
      . (Functor f1, Functor f2)
    => (forall x . f1 x -> f2 x)
    -> BracketCoOp f1 a
    -> BracketCoOp f2 a
  liftCoOp lifter (BracketOp alloc release comp) =
    BracketOp alloc release (fmap lifter comp)

instance FreeOps BracketOps where
  mkFreeOps
    :: forall eff
    . (Effect eff)
    => (forall a . BracketCoOp eff a -> eff a)
    -> BracketOps eff eff
  mkFreeOps lifter = BracketOps handler
   where
    handler
      :: forall a b
       . IO a
      -> (a -> IO ())
      -> (a -> eff b)
      -> eff b
    handler alloc release comp = lifter $
      BracketOp alloc release comp

bracketCoOpHandler
  :: CoOpHandler BracketOps IO Identity
bracketCoOpHandler = CoOpHandler
  (return . Identity) handleOp contraIdentity
 where
  handleOp
    :: BracketCoOp (IO ∘ Identity) a
    -> (a -> IO (Identity r))
    -> IO (Identity r)
  handleOp (BracketOp alloc release comp1) cont = do
    Identity a <- bracket alloc release (fmap unNest comp1)
    cont a
