module Control.Effect.Implicit.Ops.Resource
where

import Data.Kind
import Control.Exception

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation

import Control.Effect.Implicit.Ops.Io
import Control.Effect.Implicit.Ops.UnliftIo

type FixedEff fix ops eff = fix ops eff ∪ ops

data BracketTask a = BracketTask {
  allocateResource :: IO a,
  releaseResource :: a -> IO ()
}

data ResourceEff
  (t :: Type -> Type)
  (ops :: Type)

data FixedResourceEff
  (t :: Type -> Type)
  (ops :: Type)

data ResourceOps t ops eff = ResourceOps
  { withResourceOp
      :: forall a b
       . (Effect eff, ImplicitOps ops)
      => t a
      -> (forall eff2
           . (EffConstraint ops eff2)
          => a -> eff2 b)
      -> eff b
  }

data FixedResourceOps t ops eff = FixedResourceOps {
  unFixedResourceOps
    :: ResourceOps t (FixedResourceEff t ops ∪ ops) eff
}

data ResourceCoOp
  :: (Type -> Type)
  -> Type
  -> Type
  -> Type
 where
  WithResourceOp
    :: forall t ops a b r
     . t a
    -> (forall eff
         . (EffConstraint ops eff)
        => a -> eff b)
    -> (b -> r)
    -> ResourceCoOp t ops r

instance EffOps (ResourceEff t ops) where
  type Operation (ResourceEff t ops)
    = ResourceOps t ops

instance EffOps (FixedResourceEff t ops) where
  type Operation (FixedResourceEff t ops)
    = FixedResourceOps t ops

instance EffFunctor (ResourceOps t ops) where
  effmap :: forall eff1 eff2 .
    (Effect eff1, Effect eff2)
    => (forall x . eff1 x -> eff2 x)
    -> ResourceOps t ops eff1
    -> ResourceOps t ops eff2
  effmap lift ops = ResourceOps withResourceOp'
   where
    withResourceOp'
      :: forall a b
       . (ImplicitOps ops)
      => t a
      -> (forall eff
           . (EffConstraint ops eff)
          => a -> eff b)
      -> eff2 b
    withResourceOp' task cont = lift $
      withResourceOp ops task cont

instance EffFunctor (FixedResourceOps ops res) where
  effmap lift (FixedResourceOps ops) =
    FixedResourceOps $ effmap lift ops

instance ImplicitOps (ResourceEff t ops) where
  type OpsConstraint (ResourceEff t ops) eff
    = (?_Control_Effects_Implicit_Ops_Resource_resourceOps
        :: ResourceOps t ops eff)

  withOps ops cont =
    let
      ?_Control_Effects_Implicit_Ops_Resource_resourceOps
        = ops in cont

  captureOps =
    ?_Control_Effects_Implicit_Ops_Resource_resourceOps

instance ImplicitOps (FixedResourceEff t ops) where
  type OpsConstraint (FixedResourceEff t ops) eff
    = (?_Control_Effects_Implicit_Ops_Resource_fixedResourceOps
        :: FixedResourceOps t ops eff)

  withOps ops cont =
    let
      ?_Control_Effects_Implicit_Ops_Resource_fixedResourceOps
        = ops in cont

  captureOps =
    ?_Control_Effects_Implicit_Ops_Resource_fixedResourceOps

bracketResourceOps
  :: forall ops res eff
   . ( ImplicitOps ops
     , EffConstraint (IoEff ∪ UnliftIoEff res ops) eff
     )
  => ResourceOps BracketTask ops eff
bracketResourceOps = ResourceOps withResourceOp'
 where
  withResourceOp'
    :: forall a b
    .  BracketTask a
    -> (forall eff3
         . (EffConstraint ops eff3)
        => a -> eff3 b)
    -> eff b
  withResourceOp'
    (BracketTask alloc release)
    comp1
    = do
      unlifter <- unliftIoOp captureOps
      res1 <- liftIo $ bracket alloc release $
        \x -> unlifter $ GenericComp $ comp1 x
      extractIoRes res1

bracketResourceOps'
  :: forall ops res eff
    . ( ImplicitOps ops
      , EffConstraint (FixedUnliftIoEff res ops ∪ IoEff) eff
      )
  => ResourceOps BracketTask (FixedUnliftIoEff res ops ∪ ops) eff
bracketResourceOps' = withOps (unfixUnliftIoOps captureOps) $ bracketResourceOps

mkFixedResourceOps'
  :: forall ops2 ops1 t eff
    . ( ImplicitOps ops1
      , ImplicitOps ops2
      , Effect eff
      , EffConstraint ops2 eff
      )
  => ops1 ⊇ ops2
  -> (forall eff2
      . (EffConstraint ops2 eff2)
      => ResourceOps t ops1 eff2)
  -> FixedResourceOps t ops1 eff
mkFixedResourceOps' caster ops1 = ops2
 where
  ops2 :: forall eff3
     . (EffConstraint ops2 eff3)
    => FixedResourceOps t ops1 eff3
  ops2 = FixedResourceOps $ ResourceOps withResource'
   where
    withResource'
      :: forall a b
      . t a
      -> (forall eff2
           . (EffConstraint
                (FixedResourceEff t ops1 ∪ ops1)
                eff2)
          => a -> eff2 b)
      -> eff3 b
    withResource' task comp1 = withResourceOp ops1 task comp2
     where
      comp2
        :: forall eff2
         . (EffConstraint ops1 eff2)
        => a -> eff2 b
      comp2 x = withOps ops4 $ comp1 x
       where
        ops4 :: FixedResourceOps t ops1 eff2
        ops4 = withCast @eff2 @ops1 @ops2 caster $ ops2

mkFixedResourceOps
  :: forall ops t eff
   . ( ImplicitOps ops
     , Effect eff
     , EffConstraint ops eff
     )
  => (forall eff2
      . (EffConstraint ops eff2)
      => ResourceOps t ops eff2)
  -> FixedResourceOps t ops eff
mkFixedResourceOps = mkFixedResourceOps' @ops cast

withResource
  :: forall a b t ops eff
   . ( ImplicitOps ops
     , EffConstraint (ResourceEff t ops) eff)
  => t a
  -> (forall eff2
       . (EffConstraint ops eff2)
      => a
      -> eff2 b)
  -> eff b
withResource task comp1 = withResourceOp captureOps task comp1

unfixResourceOps
  :: forall t ops eff r
   . ( ImplicitOps ops
     , EffConstraint (FixedResourceEff t ops) eff
     )
  => ((OpsConstraint
        (ResourceEff t (FixedResourceEff t ops ∪ ops))
        eff)
      => eff r)
  -> eff r
unfixResourceOps comp = withOps (unFixedResourceOps captureOps) comp

withFixedResource
  :: forall a b t ops eff
   . ( ImplicitOps ops
     , EffConstraint (FixedResourceEff t ops) eff)
  => t a
  -> (forall eff2
       . (EffConstraint (FixedResourceEff t ops ∪ ops) eff2)
      => a
      -> eff2 b)
  -> eff b
withFixedResource task comp1 = unfixResourceOps $ withResource task comp1