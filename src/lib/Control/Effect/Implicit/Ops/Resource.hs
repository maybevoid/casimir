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
  (t :: Type -> Type)
  (ops :: Type)
  (eff :: Type -> Type)

data FixedResourceEff
  (t :: Type -> Type)
  (ops :: Type)
  (eff :: Type -> Type)

data ResourceOps t ops eff1 eff2 = ResourceOps
  { withResourceOp
      :: forall a b
       . (Effect eff1, Effect eff2, ImplicitOps ops)
      => t a
      -> (a -> Computation ops (Return b) eff1)
      -> eff2 b
  }

data FixedResourceOps t ops eff1 eff2 = FixedResourceOps {
  unFixedResourceOps
    :: ResourceOps t (FixedResourceEff t ops eff1 ∪ ops) eff1 eff2
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

instance EffOps (FixedResourceEff t ops eff) where
  type Operation (FixedResourceEff t ops eff)
    = FixedResourceOps t ops eff

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

instance EffFunctor (FixedResourceOps ops res eff) where
  effmap lift (FixedResourceOps ops) = FixedResourceOps $ effmap lift ops

instance ImplicitOps (ResourceEff t ops eff) where
  type OpsConstraint (ResourceEff t ops eff) eff2
    = (?_Control_Effects_Implicit_Ops_Resource_resourceOps
        :: ResourceOps t ops eff eff2)

  withOps ops cont =
    let
      ?_Control_Effects_Implicit_Ops_Resource_resourceOps
        = ops in cont

  captureOps =
    ?_Control_Effects_Implicit_Ops_Resource_resourceOps

instance ImplicitOps (FixedResourceEff t ops eff) where
  type OpsConstraint (FixedResourceEff t ops eff) eff2
    = (?_Control_Effects_Implicit_Ops_Resource_fixedResourceOps
        :: FixedResourceOps t ops eff eff2)

  withOps ops cont =
    let
      ?_Control_Effects_Implicit_Ops_Resource_fixedResourceOps
        = ops in cont

  captureOps =
    ?_Control_Effects_Implicit_Ops_Resource_fixedResourceOps

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

mkFixedResourceOps
  :: forall t ops eff
   . ( ImplicitOps ops
     , EffConstraint ops eff
     )
  => ResourceOps t ops eff eff
  -> FixedResourceOps t ops eff eff
mkFixedResourceOps (ResourceOps withResource1) = ops1
 where
  ops1 :: FixedResourceOps t ops eff eff
  ops1 = FixedResourceOps ops2
    where
    ops2 :: ResourceOps t (FixedResourceEff t ops eff ∪ ops) eff eff
    ops2 = ResourceOps withResource2

    withResource2
      :: forall a b
       . t a
      -> (a -> Computation
                (FixedResourceEff t ops eff ∪ ops)
                (Return b)
                eff)
      -> eff b
    withResource2 task comp = withResource1 task $
      \x -> bindOps ops1 $ comp x

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

withFixedResource
  :: forall a b t ops eff1 eff2
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (FixedResourceEff t ops eff1) eff2)
  => t a
  -> (forall eff
       . (EffConstraint (FixedResourceEff t ops eff1 ∪ ops) eff)
      => a
      -> eff b)
  -> eff2 b
withFixedResource task comp1 = withResourceOp ops1 task comp2
 where
  ops1 :: ResourceOps t (FixedResourceEff t ops eff1 ∪ ops) eff1 eff2
  ops1 = unFixedResourceOps captureOps

  comp2 :: a -> Computation (FixedResourceEff t ops eff1 ∪ ops) (Return b) eff1
  comp2 x = genericReturn $ comp1 x