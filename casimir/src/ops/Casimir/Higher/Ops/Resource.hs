
module Casimir.Higher.Ops.Resource
where

import Data.Kind
import Control.Monad.Identity
import Control.Exception (bracket)

import Casimir.Base
  ( ContraLift (..)
  , EffFunctor (..)
  , HigherLift (..)
  , ImplicitOps (..)
  , EffConstraint
  , HasLabel (..)
  , Tag
  , Lift (..)
  , type (~>)
  , captureOps
  )

import Casimir.Higher
import Casimir.Higher.Free
import Casimir.Higher.ContraLift.Identity

import qualified Casimir.Base as Base

data ResourceTag
data ResourceEff (t :: Type -> Type)

type ResourceOps t = LowerOps (HigherResourceOps t)

pattern ResourceOps
  :: forall t m
   . (forall a b. t a -> (a -> m b) -> m b)
  -> LowerOps (HigherResourceOps t) m
pattern ResourceOps t = LowerOps (HigherResourceOps t)

data HigherResourceOps t inEff m = HigherResourceOps {
  withResourceOp
    :: forall a b
     . t a
    -> (a -> inEff b)
    -> m b
}

data ResourceCoOp t f r where
  ResourceOp
    :: forall t f a r
     . t a
    -> (a -> f r)
    -> ResourceCoOp t f r

data BracketResource a = BracketResource
  { allocOp :: IO a
  , releaseOp :: (a -> IO ())
  }

instance Effects (ResourceEff t) where
  type Operations (ResourceEff t) = HigherResourceOps t

instance Base.Effects (ResourceEff t) where
  type Operations (ResourceEff t) = LowerOps (HigherResourceOps t)

instance HasLabel (HigherResourceOps t) where
  type GetLabel (HigherResourceOps t) = Tag ResourceTag

instance
  (Monad inEff)
  => EffFunctor Lift (HigherResourceOps t inEff) where
    effmap (Lift lift) (HigherResourceOps handleResource) =
      HigherResourceOps $
        \resource cont ->
          lift $ handleResource resource cont

instance HigherEffFunctor HigherLift (HigherResourceOps t) where
  higherEffmap
    :: forall m1 m2
      . ( Monad m1
        , Monad m2
        )
    => HigherLift m1 m2
    -> HigherResourceOps t m1 m1
    -> HigherResourceOps t m2 m2
  higherEffmap
    (HigherLift _ (ContraLift contraLift1))
    (HigherResourceOps doResource) =
      HigherResourceOps ops
       where
        ops
          :: forall a b
           . t a
          -> (a -> m2 b)
          -> m2 b
        ops resource cont1 = contraLift1 cont2
         where
          cont2
            :: forall w
             . (forall x . m2 x -> m1 (w x))
            -> m1 (w b)
          cont2 contraLift2 = doResource resource cont3
           where
            cont3 :: a -> m1 (w b)
            cont3 x = contraLift2 $ cont1 x

instance
  (Functor f)
  => Functor (ResourceCoOp t f) where
    fmap f (ResourceOp resource comp) =
      ResourceOp resource (fmap (fmap f) comp)

instance CoOpFunctor (ResourceCoOp t) where
  liftCoOp
    :: forall f1 f2 a
      . (Functor f1, Functor f2)
    => f1 ~> f2
    -> ResourceCoOp t f1 a
    -> ResourceCoOp t f2 a
  liftCoOp lifter (ResourceOp resource comp) =
    ResourceOp resource (fmap lifter comp)

instance FreeOps (HigherResourceOps t) where
  type CoOperation (HigherResourceOps t) = ResourceCoOp t

  mkFreeOps
    :: forall m
    . (Monad m)
    => (forall a . ResourceCoOp t m a -> m a)
    -> HigherResourceOps t m m
  mkFreeOps lifter = HigherResourceOps handler
   where
    handler
      :: forall a b
       . t a
      -> (a -> m b)
      -> m b
    handler resource comp = lifter $
      ResourceOp resource comp

ioBracketOps :: ResourceOps BracketResource IO
ioBracketOps = ResourceOps $
  \(BracketResource alloc release) cont ->
    bracket alloc release cont

ioBracketCoOpHandler
  :: CoOpHandler (HigherResourceOps BracketResource) Identity IO
ioBracketCoOpHandler = CoOpHandler
  (return . Identity) handleOp contraIdentity
 where
  handleOp
    :: forall a r
     . ResourceCoOp BracketResource (IO ∘ Identity) a
    -> (a -> IO (Identity r))
    -> IO (Identity r)
  handleOp (ResourceOp (BracketResource alloc release) comp1) cont = do
    Identity a <- bracket alloc release (fmap unNest comp1)
    cont a

withResource
  :: forall t m a b
   . (EffConstraint (ResourceEff t) m)
  => t a
  -> (a -> m b)
  -> m b
withResource = withResourceOp $ unLowerOps captureOps
