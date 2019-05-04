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

bracketResourceOps'
  :: forall ops res eff1 eff2
    . ( Effect eff1
      , ImplicitOps ops
      , EffConstraint (FixedUnliftIoEff ops res eff1 ∪ IoEff) eff2
      )
  => ResourceOps BracketTask (FixedUnliftIoEff ops res eff1 ∪ ops) eff1 eff2
bracketResourceOps' = withOps (unfixUnliftIoOps captureOps) $ bracketResourceOps

mkFixedResourceOps
  :: forall t ops eff
   . ( Effect eff
     , ImplicitOps ops
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

mkFixedResourceOps'
  :: forall ops2 ops1 t eff1 eff2
    . ( ImplicitOps ops1
      , ImplicitOps ops2
      , Effect eff1
      , Effect eff2
      , EffConstraint ops2 eff2
      )
  => ops1 ⊇ ops2
  -> (forall eff
      . (EffConstraint ops2 eff)
      => ResourceOps t ops1 eff1 eff)
  -> FixedResourceOps t ops1 eff1 eff2
mkFixedResourceOps' caster ops1 = ops2
 where
  ops2 :: forall eff3
     . (EffConstraint ops2 eff3)
    => FixedResourceOps t ops1 eff1 eff3
  ops2 = FixedResourceOps ops3
   where
    ops3 :: ResourceOps t (FixedResourceEff t ops1 eff1 ∪ ops1) eff1 eff3
    ops3 = ResourceOps withResource'

    ops4 :: ResourceOps t ops1 eff1 eff3
    ops4 = ops1

    withResource'
      :: forall a b
      . t a
      -> (a
          -> Computation
              (FixedResourceEff t ops1 eff1 ∪ ops1)
              (Return b)
              eff1)
      -> eff3 b
    withResource' task comp1 = withResourceOp ops4 task comp2
     where
      comp2 :: a -> Computation ops1 (Return b) eff1
      comp2 x = Computation comp3
       where
        comp3 :: forall eff4 
           . (Effect eff4)
          => LiftEff eff1 eff4
          -> Operation ops1 eff4
          -> Return b eff4
        comp3 lift13 ops5 = runComp (comp1 x) lift13 (ops6 ∪ ops5)
         where
          ops6 :: FixedResourceOps t ops1 eff1 eff4
          ops6 = withOps ops7 $ ops2

          ops7 :: Operation ops2 eff4
          ops7 = castOps caster ops5

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

unfixResourceOps
  :: forall t ops eff1 eff2 r
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (FixedResourceEff t ops eff1) eff2
     )
  => ((OpsConstraint
        (ResourceEff t (FixedResourceEff t ops eff1 ∪ ops) eff1)
        eff2)
      => eff2 r)
  -> eff2 r
unfixResourceOps comp = withOps (unFixedResourceOps captureOps) comp

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
withFixedResource task comp1 = unfixResourceOps $ withResource task comp1