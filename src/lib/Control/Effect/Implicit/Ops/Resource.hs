module Control.Effect.Implicit.Ops.Resource
where

import Data.Kind
import Control.Exception

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation

import Control.Effect.Implicit.Ops.Io
import Control.Effect.Implicit.Ops.Env

data ResourceEff
  :: (Type -> Type)
  -> Type
  -> (Type -> Type)
  -> Type
  where

data ResourceOps t inOps inEff eff = ResourceOps
  { withResourceOp
      :: forall a b
       . (Effect inEff, ImplicitOps inOps)
      => t a
      -> Computation (EnvEff a ∪ inOps) (Return b) inEff
      -> eff b
  }

data ResourceCoOp
  :: (Type -> Type)
  -> Type
  -> (Type -> Type)
  -> Type
  -> Type
 where
  WithResourceOp
    :: forall t inOps inEff a b r
     . t a
    -> Computation (EnvEff a ∪ inOps) (Return b) inEff
    -> (b -> r)
    -> ResourceCoOp t inOps inEff r

instance EffOps (ResourceEff t inOps inEff) where
  type Operation (ResourceEff t inOps inEff)
    = ResourceOps t inOps inEff

instance ImplicitOps (ResourceEff t inOps inEff) where
  type OpsConstraint (ResourceEff t inOps inEff) eff
    = (?_Control_Effects_Implicit_Ops_Resource_resourceOps
        :: ResourceOps t inOps inEff eff)

  withOps ops cont =
    let
      ?_Control_Effects_Implicit_Ops_Resource_resourceOps
        = ops in cont

  captureOps = ?_Control_Effects_Implicit_Ops_Resource_resourceOps

instance EffFunctor (ResourceOps t inOps inEff) where
  effmap :: forall eff1 eff2 .
    (Effect eff1, Effect eff2)
    => (forall x . eff1 x -> eff2 x)
    -> ResourceOps t inOps inEff eff1
    -> ResourceOps t inOps inEff eff2
  effmap lift ops = ResourceOps withResourceOp'
   where
    withResourceOp'
      :: forall a b
       . (Effect inEff, ImplicitOps inOps)
      => t a
      -> Computation (EnvEff a ∪ inOps) (Return b) inEff
      -> eff2 b
    withResourceOp' task cont = lift $
      withResourceOp ops task cont

data BracketTask a = BracketTask {
  allocateResource :: IO a,
  releaseResource :: a -> IO ()
}

bracketResourceOpsHandler
  :: forall extractOps f inOps inEff eff
   . ( Effect inEff
     , Effect eff
     , ImplicitOps extractOps
     , ImplicitOps inOps
     )
  => (forall x . Pipeline NoEff inOps (Return x) (Return (f x)) inEff IO)
  -> (forall x . Computation (EnvEff (f x) ∪ extractOps) (Return x) eff)
  -> Computation (IoEff ∪ extractOps) (ResourceOps BracketTask inOps inEff) eff
bracketResourceOpsHandler unliftPipeline extractComp
  = Computation comp
 where
  comp
    :: forall eff2
     . (Effect eff2)
    => LiftEff eff eff2
    -> Operation (IoEff ∪ extractOps) eff2
    -> ResourceOps BracketTask inOps inEff eff2
  comp lift12 (UnionOps ioOps' extractOps) = ResourceOps withResourceOp'
   where
    withResourceOp'
      :: forall a b
       . BracketTask a
      -> Computation (EnvEff a ∪ inOps) (Return b) inEff
      -> eff2 b
    withResourceOp'
      (BracketTask alloc release)
      comp1 = do
        res1 <- liftIoOp ioOps' $ bracket alloc release $
          \x -> returnVal $ runComp (comp2 x) idLift NoOp

        returnVal $ runComp extractComp lift12 $
          mkEnvOps res1 ∪ extractOps

       where
        comp2 :: a -> Computation NoEff (Return (f b)) IO
        comp2 x = runPipelineWithCast
          cast cast
          unliftPipeline $
          bindOps (mkEnvOps x) comp1

-- ioResourceOps
--   :: forall inOps
--    . (ImplicitOps inOps)
--   => Operation inOps IO
--   -> ResourceOps BracketTask inOps IO IO
-- ioResourceOps ops = runComp comp idLift (ioOps ∪ NoOp)
--  where
--   comp = bracketResourceOps
--     (fmap Identity)
--     (genericReturn $ ask >>= return . runIdentity)
--     ops

-- ioResourceOpsComp
--   :: forall inOps
--    . (ImplicitOps inOps)
--   => Operation inOps IO
--   -> Computation NoEff (ResourceOps BracketTask inOps IO) IO
-- ioResourceOpsComp ops = baseOpsHandler $ ioResourceOps ops