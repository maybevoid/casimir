module Control.Effect.Implicit.Ops.Resource
where

import Data.Kind
import Control.Exception

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation

import Control.Effect.Implicit.Ops.Io
import Control.Effect.Implicit.Ops.UnliftIo

data BracketTask a = BracketTask {
  allocateResource :: IO a,
  releaseResource :: a -> IO ()
}

data ResourceEff
  :: (Type -> Type)
  -> Type
  -> (Type -> Type)
  -> Type
  where

data ResourceOps t ops eff1 eff2 = ResourceOps
  { withResourceOp
      :: forall a b
       . (Effect eff1, Effect eff2, ImplicitOps ops)
      => t a
      -> (a -> Computation ops (Return b) eff1)
      -> eff2 b
  }

data ResourceCoOp
  :: (Type -> Type)
  -> Type
  -> (Type -> Type)
  -> Type
  -> Type
 where
  WithResourceOp
    :: forall t ops eff a b r
     . t a
    -> (a -> Computation ops (Return b) eff)
    -> (b -> r)
    -> ResourceCoOp t ops eff r

instance EffOps (ResourceEff t ops eff) where
  type Operation (ResourceEff t ops eff)
    = ResourceOps t ops eff

instance EffFunctor (ResourceOps t ops inEff) where
  effmap :: forall eff1 eff2 .
    (Effect eff1, Effect eff2)
    => (forall x . eff1 x -> eff2 x)
    -> ResourceOps t ops inEff eff1
    -> ResourceOps t ops inEff eff2
  effmap lift ops = ResourceOps withResourceOp'
   where
    withResourceOp'
      :: forall a b
       . (Effect inEff, ImplicitOps ops)
      => t a
      -> (a -> Computation ops (Return b) inEff)
      -> eff2 b
    withResourceOp' task cont = lift $
      withResourceOp ops task cont

instance ImplicitOps (ResourceEff t ops eff) where
  type OpsConstraint (ResourceEff t ops eff) eff2
    = (?_Control_Effects_Implicit_Ops_Resource_resourceOps
        :: ResourceOps t ops eff eff2)

  withOps ops cont =
    let
      ?_Control_Effects_Implicit_Ops_Resource_resourceOps
        = ops in cont

  captureOps = ?_Control_Effects_Implicit_Ops_Resource_resourceOps

bracketResourceOps
  :: forall ops res eff1 eff2
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (IoEff ∪ UnliftIoEff ops res eff1) eff2
     )
  => ResourceOps BracketTask ops eff1 eff2
bracketResourceOps = ResourceOps withResourceOp'
 where
  withResourceOp'
    :: forall a b
    .  BracketTask a
    -> (a -> Computation ops (Return b) eff1)
    -> eff2 b
  withResourceOp'
    (BracketTask alloc release)
    comp1
    = do
      unlifter <- unliftIoOp captureOps
      res1 <- liftIo $ bracket alloc release $ unlifter . comp1
      extractIoRes res1

withResource
  :: forall a b t ops eff1 eff2
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (ResourceEff t ops eff1) eff2)
  => t a
  -> (forall eff
       . (EffConstraint ops eff)
      => a
      -> eff b)
  -> eff2 b
withResource task comp1 = withResourceOp captureOps task comp2
 where
  comp2 :: a -> Computation ops (Return b) eff1
  comp2 x = genericReturn $ comp1 x
