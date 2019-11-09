
module Control.Effect.Implicit.Higher.Ops.Resource
where

import Data.Kind
import Control.Monad.Identity
import Control.Exception (bracket)

import Control.Implicit.Param
import Control.Effect.Implicit.Higher
import Control.Effect.Implicit.Higher.Free
import Control.Effect.Implicit.Higher.ContraLift.Identity

import qualified Control.Effect.Implicit.Base as Base

data ResourceEff (t :: Type -> Type)
data ResourceTag

data ResourceOps t inEff eff = ResourceOps {
  withResourceOp
    :: forall a b
     . t a
    -> (a -> inEff b)
    -> eff b
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

instance EffOps (ResourceEff t) where
  type Operation (ResourceEff t) = ResourceOps t

instance ImplicitOps (ResourceEff t) where
  type OpsConstraint (ResourceEff t) eff1 eff2 =
    TaggedParam ResourceTag (ResourceOps t eff1 eff2)

  withHigherOps = withTag @ResourceTag
  captureHigherOps = captureTag @ResourceTag

instance
  (Effect inEff)
  => Base.EffFunctor (ResourceOps t inEff) where
    effmap lifter (ResourceOps handleResource) = ResourceOps $
      \resource cont ->
        lifter $ handleResource resource cont

instance EffFunctor (ResourceOps t) where
  invEffmap
    :: forall eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => (forall x . eff1 x -> eff2 x)
    -> ContraLift eff1 eff2
    -> ResourceOps t eff1 eff1
    -> ResourceOps t eff2 eff2
  invEffmap _
    (ContraLift contraLift1)
    (ResourceOps doResource) =
      ResourceOps ops
       where
        ops
          :: forall a b
           . t a
          -> (a -> eff2 b)
          -> eff2 b
        ops resource cont1 = contraLift1 cont2
         where
          cont2
            :: forall w
             . (forall x . eff2 x -> eff1 (w x))
            -> eff1 (w b)
          cont2 contraLift2 = doResource resource cont3
           where
            cont3 :: a -> eff1 (w b)
            cont3 x = contraLift2 $ cont1 x

instance EffCoOp (ResourceEff t) where
  type CoOperation (ResourceEff t) = ResourceCoOp t

instance
  (Functor f)
  => Functor (ResourceCoOp t f) where
    fmap f (ResourceOp resource comp) =
      ResourceOp resource (fmap (fmap f) comp)

instance CoOpFunctor (ResourceCoOp t) where
  liftCoOp
    :: forall f1 f2 a
      . (Functor f1, Functor f2)
    => (forall x . f1 x -> f2 x)
    -> ResourceCoOp t f1 a
    -> ResourceCoOp t f2 a
  liftCoOp lifter (ResourceOp resource comp) =
    ResourceOp resource (fmap lifter comp)

instance FreeOps (ResourceEff t) where
  mkFreeOps
    :: forall eff
    . (Effect eff)
    => (forall a . ResourceCoOp t eff a -> eff a)
    -> ResourceOps t eff eff
  mkFreeOps lifter = ResourceOps handler
   where
    handler
      :: forall a b
       . t a
      -> (a -> eff b)
      -> eff b
    handler resource comp = lifter $
      ResourceOp resource comp

ioBracketOps :: ResourceOps BracketResource IO IO
ioBracketOps = ResourceOps $
  \(BracketResource alloc release) cont ->
    bracket alloc release cont

ioBracketCoOpHandler
  :: CoOpHandler (ResourceEff BracketResource) IO Identity
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
  :: forall t eff1 eff2 a b
   . (EffConstraint (ResourceEff t) eff1 eff2)
  => t a
  -> (a -> eff1 b)
  -> eff2 b
withResource = withResourceOp captureHigherOps