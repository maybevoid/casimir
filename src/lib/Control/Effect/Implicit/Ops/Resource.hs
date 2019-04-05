module Control.Effect.Implicit.Ops.Resource
where

import Data.Kind
import Control.Exception
import Control.Monad.Identity

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation
import Control.Effect.Implicit.Ops.Io

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
      -> ( Operation inOps inEff
           -> a
           -> inEff b
         )
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
    -> ( Operation inOps inEff
         -> a
         -> inEff b
       )
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
      -> ( Operation inOps inEff
           -> a
           -> inEff b
         )
      -> eff2 b
    withResourceOp' task cont = lift $
      withResourceOp ops task cont

data BracketTask a = BracketTask {
  allocateResource :: IO a,
  releaseResource :: a -> IO ()
}

bracketResourceOps
  :: forall ops f inOps inEff eff
   . ( Effect inEff
     , Effect eff
     , ImplicitOps ops
     )
  => (forall x . inEff x -> IO (f x))
  -> (forall x eff2
      . (EffConstraint ops eff2)
      => f x
      -> eff2 x)
  -> Operation inOps inEff
  -> Computation (IoEff ∪ ops) (ResourceOps BracketTask inOps inEff) eff
bracketResourceOps unliftIo extractRes ops = genericComputation comp
 where
  comp
    :: forall eff2
     . (EffConstraint (IoEff ∪ ops) eff2)
    => ResourceOps BracketTask inOps inEff eff2
  comp = ResourceOps withResourceOp'
   where
    withResourceOp'
      :: forall a b
       . BracketTask a
      -> ( Operation inOps inEff
           -> a
           -> inEff b
         )
      -> eff2 b
    withResourceOp'
      (BracketTask alloc release)
      cont
      = do
        res1 <- liftIo $ bracket alloc release $
          \x -> unliftIo $ cont ops x

        extractRes res1

ioResourceOps
  :: forall inOps
   . (ImplicitOps inOps)
  => Operation inOps IO
  -> ResourceOps BracketTask inOps IO IO
ioResourceOps ops = runComp comp idLift (ioOps ∪ NoOp)
 where
  comp = bracketResourceOps
    (fmap Identity)
    (return . runIdentity)
    ops

ioResourceOpsComp
  :: forall inOps
   . (ImplicitOps inOps)
  => Operation inOps IO
  -> Computation NoEff (ResourceOps BracketTask inOps IO) IO
ioResourceOpsComp ops = baseOpsHandler $ ioResourceOps ops