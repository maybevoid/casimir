
module Casimir.Higher.Ops.Resource
where

import Data.Kind
import Control.Monad.Identity
import Control.Exception (bracket)

import Data.QuasiParam
import Casimir.Base
  ( ContraLift (..)
  , EffFunctor (..)
  , type (~>)
  )

import Casimir.Higher
import Casimir.Higher.Free
import Casimir.Higher.ContraLift.Identity

import Casimir.Base.Implicit
import qualified Casimir.Base as Base

data ResourceEff (t :: Type -> Type)
data ResourceTag

type ResourceOps t = LowerOps (HigherResourceOps t)

pattern ResourceOps
  :: forall t eff
   . (forall a b. t a -> (a -> eff b) -> eff b)
  -> LowerOps (HigherResourceOps t) eff
pattern ResourceOps t = LowerOps (HigherResourceOps t)

data HigherResourceOps t inEff eff = HigherResourceOps {
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
  type Operation (ResourceEff t) = HigherResourceOps t

instance Base.EffOps (ResourceEff t) where
  type Operation (ResourceEff t) = LowerOps (HigherResourceOps t)

instance LowerEffOps (ResourceEff t)

instance ImplicitOps (ResourceEff t) where
  type OpsConstraint (ResourceEff t) eff =
    TaggedParam ResourceTag (LowerOps (HigherResourceOps t) eff)

  withOps = withTag @ResourceTag
  captureOps = captureTag @ResourceTag

instance
  (Effect inEff)
  => EffFunctor (HigherResourceOps t inEff) where
    effmap lifter (HigherResourceOps handleResource) =
      HigherResourceOps $
        \resource cont ->
          lifter $ handleResource resource cont

instance HigherEffFunctor (HigherResourceOps t) where
  invEffmap
    :: forall eff1 eff2
      . ( Effect eff1
        , Effect eff2
        )
    => eff1 ~> eff2
    -> ContraLift eff1 eff2
    -> HigherResourceOps t eff1 eff1
    -> HigherResourceOps t eff2 eff2
  invEffmap _
    (ContraLift contraLift1)
    (HigherResourceOps doResource) =
      HigherResourceOps ops
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
    => f1 ~> f2
    -> ResourceCoOp t f1 a
    -> ResourceCoOp t f2 a
  liftCoOp lifter (ResourceOp resource comp) =
    ResourceOp resource (fmap lifter comp)

instance FreeOps (ResourceEff t) where
  mkFreeOps
    :: forall eff
    . (Effect eff)
    => (forall a . ResourceCoOp t eff a -> eff a)
    -> HigherResourceOps t eff eff
  mkFreeOps lifter = HigherResourceOps handler
   where
    handler
      :: forall a b
       . t a
      -> (a -> eff b)
      -> eff b
    handler resource comp = lifter $
      ResourceOp resource comp

ioBracketOps :: ResourceOps BracketResource IO
ioBracketOps = ResourceOps $
  \(BracketResource alloc release) cont ->
    bracket alloc release cont

ioBracketCoOpHandler
  :: CoOpHandler (ResourceEff BracketResource) Identity IO
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
  :: forall t eff a b
   . (EffConstraint (ResourceEff t) eff)
  => t a
  -> (a -> eff b)
  -> eff b
withResource = withResourceOp $ unLowerOps captureOps
