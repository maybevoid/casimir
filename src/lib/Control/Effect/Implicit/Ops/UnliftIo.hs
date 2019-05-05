module Control.Effect.Implicit.Ops.UnliftIo
where

import Data.Kind
import Control.Monad.Identity

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation
import Control.Effect.Implicit.Ops.Io

data UnliftIoEff
  (res :: Type -> Type)
  (ops :: Type)
  (eff :: Type -> Type)

data FixedUnliftIoEff
  (res :: Type -> Type)
  (ops :: Type)
  (eff :: Type -> Type)

data UnliftIoOps res ops eff1 eff2 = UnliftIoOps {
  unliftIoOp
    :: forall a
     . eff2 (Computation ops (Return a) eff1 -> IO (res a))
  ,
  extractIoResOp :: forall a . res a -> eff2 a
}

data UnliftIoCoOp res ops eff r where
  UnliftIoOp
    :: forall res ops eff r a
     . ((Computation ops (Return a) eff -> IO (res a)) -> r)
     -> UnliftIoCoOp res ops eff r

  ExtractIoResOp
    :: forall res ops eff r a
     . res a
    -> (a -> r)
    -> UnliftIoCoOp res ops eff r

data FixedUnliftIoOps res ops eff1 eff2 = FixedUnliftIoOps {
  unfixUnliftIoOps
    :: UnliftIoOps res (FixedUnliftIoEff res ops eff1 ∪ ops) eff1 eff2
}

instance EffOps (UnliftIoEff res ops eff) where
  type Operation (UnliftIoEff res ops eff) = UnliftIoOps res ops eff

instance EffCoOp (UnliftIoEff res ops eff) where
  type CoOperation (UnliftIoEff res ops eff) = UnliftIoCoOp res ops eff

instance EffOps (FixedUnliftIoEff res ops eff) where
  type Operation (FixedUnliftIoEff res ops eff) = FixedUnliftIoOps res ops eff

instance Functor (UnliftIoCoOp res ops eff) where
  fmap f (UnliftIoOp cont) = UnliftIoOp $ fmap f cont
  fmap f (ExtractIoResOp res cont) = ExtractIoResOp res $ fmap f cont

instance EffFunctor (UnliftIoOps res ops eff) where
  effmap lift ops = UnliftIoOps {
    unliftIoOp = lift $ unliftIoOp ops,
    extractIoResOp = \res -> lift $ extractIoResOp ops res
  }

instance EffFunctor (FixedUnliftIoOps res ops eff) where
  effmap lift (FixedUnliftIoOps ops) = FixedUnliftIoOps $ effmap lift ops

instance FreeOps (UnliftIoEff res ops eff) where
  mkFreeOps liftCoOp = UnliftIoOps {
    unliftIoOp = liftCoOp $ UnliftIoOp id,
    extractIoResOp = \res -> liftCoOp $ ExtractIoResOp res id
  }

instance ImplicitOps (UnliftIoEff res ops eff1) where
  type OpsConstraint (UnliftIoEff res ops eff1) eff2 =
    (?_Control_Effect_Implicit_Ops_UnliftIo_unliftIoOps
      :: UnliftIoOps res ops eff1 eff2)

  withOps ops comp =
    let
      ?_Control_Effect_Implicit_Ops_UnliftIo_unliftIoOps =
        ops in comp

  captureOps =
    ?_Control_Effect_Implicit_Ops_UnliftIo_unliftIoOps

instance ImplicitOps (FixedUnliftIoEff res ops eff1) where
  type OpsConstraint (FixedUnliftIoEff res ops eff1) eff2 =
    (?_Control_Effect_Implicit_Ops_UnliftIo_fixedUnliftIoOps
      :: FixedUnliftIoOps res ops eff1 eff2)

  withOps ops comp =
    let
      ?_Control_Effect_Implicit_Ops_UnliftIo_fixedUnliftIoOps =
        ops in comp

  captureOps =
    ?_Control_Effect_Implicit_Ops_UnliftIo_fixedUnliftIoOps

unliftIo
  :: forall eff2 a res ops eff1
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (UnliftIoEff res ops eff1) eff2
     )
  => (forall eff . (EffConstraint ops eff) => eff a)
  -> eff2 (IO (res a))
unliftIo comp = unliftIoOp captureOps <*> pure (genericReturn comp)

extractIoRes
  :: forall a res ops eff1 eff2
   . ( Effect eff1
     , EffConstraint (UnliftIoEff res ops eff1) eff2
     )
  => res a
  -> eff2 a
extractIoRes = extractIoResOp captureOps

runInIo
  :: forall res ops eff1 eff2 a
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (UnliftIoEff res ops eff1 ∪ IoEff) eff2
     )
  => (forall eff . (EffConstraint ops eff) => eff a)
  -> eff2 a
runInIo comp = do
  action <- unliftIo comp
  res <- liftIo action
  extractIoRes res

mkUnliftIoOps
  :: forall res ops eff1 eff2
   . ( Effect eff1
     , Effect eff2
     , ImplicitOps ops
     )
  => (forall a . eff2 (Computation ops (Return a) eff1 -> (IO (res a))))
  -> (forall a . res a -> eff2 a)
  -> UnliftIoOps res ops eff1 eff2
mkUnliftIoOps = UnliftIoOps

mkFixedUnliftIoOps
  :: forall res ops eff1
   . ( ImplicitOps ops
     , EffConstraint ops eff1
     )
  => UnliftIoOps res ops eff1 eff1
  -> FixedUnliftIoOps res ops eff1 eff1
mkFixedUnliftIoOps (UnliftIoOps unlift1 extract1) = ops1
 where
  ops1 :: FixedUnliftIoOps res ops eff1 eff1
  ops1 = FixedUnliftIoOps ops2
   where
    ops2 :: UnliftIoOps res (FixedUnliftIoEff res ops eff1 ∪ ops) eff1 eff1
    ops2 = UnliftIoOps unlift2 extract1

    unlift2 :: forall a
       . eff1 (Computation (FixedUnliftIoEff res ops eff1 ∪ ops) (Return a) eff1
              -> IO (res a))
    unlift2 = do
      unlift3 <- unlift1
      return $ \comp -> unlift3 $ bindOps ops1 comp

mkFixedUnliftIoOps'
  :: forall ops2 ops1 res eff1 eff2
    . ( ImplicitOps ops1
      , ImplicitOps ops2
      , Effect eff1
      , Effect eff2
      , EffConstraint ops2 eff2
      )
  => ops1 ⊇ ops2
  -> (forall eff
      . (EffConstraint ops2 eff)
      => UnliftIoOps res ops1 eff1 eff)
  -> FixedUnliftIoOps res ops1 eff1 eff2
mkFixedUnliftIoOps' caster ops1 = ops2
 where
  ops2 :: forall eff3
     . (EffConstraint ops2 eff3)
    => FixedUnliftIoOps res ops1 eff1 eff3
  ops2 = FixedUnliftIoOps $ UnliftIoOps unlift1 $ extractIoResOp ops1
   where
    unlift1 :: forall a
        . eff3
            (Computation
              (FixedUnliftIoEff res ops1 eff1 ∪ ops1)
              (Return a)
              eff1
             -> IO (res a))
    unlift1 =  do
      unlift2 <- unliftIoOp ops1
      return $ \comp1 ->
        let
          comp2 :: Computation ops1 (Return a) eff1
          comp2 = Computation comp3

          comp3 :: forall eff4 . (Effect eff4)
            => LiftEff eff1 eff4
            -> Operation ops1 eff4
            -> Return a eff4
          comp3 lift13 ops3 = runComp comp1 lift13 (ops4 ∪ ops3)
           where
            ops4 :: FixedUnliftIoOps res ops1 eff1 eff4
            ops4 = withOps ops5 $ ops2

            ops5 :: Operation ops2 eff4
            ops5 = castOps caster ops3
        in
        unlift2 comp2

unfixUnliftIo
  :: forall res ops eff1 eff2 r
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (FixedUnliftIoEff res ops eff1) eff2
     )
  => ((OpsConstraint
        (UnliftIoEff res (FixedUnliftIoEff res ops eff1 ∪ ops) eff1)
        eff2)
      => eff2 r)
  -> eff2 r
unfixUnliftIo comp = withOps (unfixUnliftIoOps captureOps) $ comp

fixedUnliftIo
  :: forall a res ops eff1 eff2
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (FixedUnliftIoEff res ops eff1) eff2
     )
  => (forall eff
       . (EffConstraint (FixedUnliftIoEff res ops eff1 ∪ ops) eff)
      => eff a)
  -> eff2 (IO (res a))
fixedUnliftIo comp = unfixUnliftIo $ unliftIo comp

fixedRunInIo
  :: forall a res ops eff1 eff2
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (FixedUnliftIoEff res ops eff1 ∪ IoEff) eff2
     )
  => (forall eff
       . (EffConstraint (FixedUnliftIoEff res ops eff1 ∪ ops) eff)
      => eff a)
  -> eff2 a
fixedRunInIo comp = unfixUnliftIo $ runInIo comp

ioUnliftIoOps :: FixedUnliftIoOps Identity IoEff IO IO
ioUnliftIoOps = withOps ioOps $ mkFixedUnliftIoOps ops
 where
  ops :: UnliftIoOps Identity IoEff IO IO
  ops = mkUnliftIoOps (return unlift') (return . runIdentity)

  unlift' :: Computation IoEff (Return a) IO -> (IO (Identity a))
  unlift' comp = fmap Identity $ returnVal $ runComp comp idLift ioOps