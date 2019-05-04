module Control.Effect.Implicit.Ops.UnliftIo
where

import Data.Kind
import Control.Monad.Identity

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Computation
import Control.Effect.Implicit.Ops.Io

data UnliftIoEff
  (ops :: Type)
  (res :: Type -> Type)
  (eff :: Type -> Type)

data FixedUnliftIoEff
  (ops :: Type)
  (res :: Type -> Type)
  (eff :: Type -> Type)

data UnliftIoOps ops res eff1 eff2 = UnliftIoOps {
  unliftIoOp
    :: forall a
     . eff2 (Computation ops (Return a) eff1 -> IO (res a))
  ,
  extractIoResOp :: forall a . res a -> eff2 a
}

data UnliftIoCoOp ops res eff r where
  UnliftIoOp
    :: forall ops res eff r a
     . ((Computation ops (Return a) eff -> IO (res a)) -> r)
     -> UnliftIoCoOp ops res eff r

  ExtractIoResOp
    :: forall ops res eff r a
     . res a
    -> (a -> r)
    -> UnliftIoCoOp ops res eff r

data FixedUnliftIoOps ops res eff1 eff2 = FixedUnliftIoOps {
  unfixUnliftIoOps
    :: UnliftIoOps (FixedUnliftIoEff ops res eff1 ∪ ops) res eff1 eff2
}

instance EffOps (UnliftIoEff ops res eff) where
  type Operation (UnliftIoEff ops res eff) = UnliftIoOps ops res eff

instance EffCoOp (UnliftIoEff ops res eff) where
  type CoOperation (UnliftIoEff ops res eff) = UnliftIoCoOp ops res eff

instance EffOps (FixedUnliftIoEff ops res eff) where
  type Operation (FixedUnliftIoEff ops res eff) = FixedUnliftIoOps ops res eff

instance Functor (UnliftIoCoOp ops res eff) where
  fmap f (UnliftIoOp cont) = UnliftIoOp $ fmap f cont
  fmap f (ExtractIoResOp res cont) = ExtractIoResOp res $ fmap f cont

instance EffFunctor (UnliftIoOps ops res eff) where
  effmap lift ops = UnliftIoOps {
    unliftIoOp = lift $ unliftIoOp ops,
    extractIoResOp = \res -> lift $ extractIoResOp ops res
  }

instance EffFunctor (FixedUnliftIoOps ops res eff) where
  effmap lift (FixedUnliftIoOps ops) = FixedUnliftIoOps $ effmap lift ops

instance FreeOps (UnliftIoEff ops res eff) where
  mkFreeOps liftCoOp = UnliftIoOps {
    unliftIoOp = liftCoOp $ UnliftIoOp id,
    extractIoResOp = \res -> liftCoOp $ ExtractIoResOp res id
  }

instance ImplicitOps (UnliftIoEff ops res eff1) where
  type OpsConstraint (UnliftIoEff ops res eff1) eff2 =
    (?_Control_Effect_Implicit_Ops_UnliftIo_unliftIoOps
      :: UnliftIoOps ops res eff1 eff2)

  withOps ops comp =
    let
      ?_Control_Effect_Implicit_Ops_UnliftIo_unliftIoOps =
        ops in comp

  captureOps =
    ?_Control_Effect_Implicit_Ops_UnliftIo_unliftIoOps

instance ImplicitOps (FixedUnliftIoEff ops res eff1) where
  type OpsConstraint (FixedUnliftIoEff ops res eff1) eff2 =
    (?_Control_Effect_Implicit_Ops_UnliftIo_fixedUnliftIoOps
      :: FixedUnliftIoOps ops res eff1 eff2)

  withOps ops comp =
    let
      ?_Control_Effect_Implicit_Ops_UnliftIo_fixedUnliftIoOps =
        ops in comp

  captureOps =
    ?_Control_Effect_Implicit_Ops_UnliftIo_fixedUnliftIoOps

unliftIo
  :: forall eff2 a ops res eff1
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (UnliftIoEff ops res eff1) eff2
     )
  => (forall eff . (EffConstraint ops eff) => eff a)
  -> eff2 (IO (res a))
unliftIo comp = unliftIoOp captureOps <*> pure (genericReturn comp)

extractIoRes
  :: forall a ops res eff1 eff2
   . ( Effect eff1
     , EffConstraint (UnliftIoEff ops res eff1) eff2
     )
  => res a
  -> eff2 a
extractIoRes = extractIoResOp captureOps

runInIo
  :: forall ops res eff1 eff2 a
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (UnliftIoEff ops res eff1 ∪ IoEff) eff2
     )
  => (forall eff . (EffConstraint ops eff) => eff a)
  -> eff2 a
runInIo comp = do
  action <- unliftIo comp
  res <- liftIo action
  extractIoRes res

mkUnliftIoOps
  :: forall ops res eff1 eff2
   . ( Effect eff1
     , Effect eff2
     , ImplicitOps ops
     )
  => (forall a . eff2 (Computation ops (Return a) eff1 -> (IO (res a))))
  -> (forall a . res a -> eff2 a)
  -> UnliftIoOps ops res eff1 eff2
mkUnliftIoOps = UnliftIoOps

mkFixedUnliftIoOps
  :: forall ops res eff1
   . ( ImplicitOps ops
     , EffConstraint ops eff1
     )
  => UnliftIoOps ops res eff1 eff1
  -> FixedUnliftIoOps ops res eff1 eff1
mkFixedUnliftIoOps (UnliftIoOps unlift1 extract1) = ops1
 where
  ops1 :: FixedUnliftIoOps ops res eff1 eff1
  ops1 = FixedUnliftIoOps ops2
   where
    ops2 :: UnliftIoOps (FixedUnliftIoEff ops res eff1 ∪ ops) res eff1 eff1
    ops2 = UnliftIoOps unlift2 extract1

    unlift2 :: forall a
       . eff1 (Computation (FixedUnliftIoEff ops res eff1 ∪ ops) (Return a) eff1
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
      => UnliftIoOps ops1 res eff1 eff)
  -> FixedUnliftIoOps ops1 res eff1 eff2
mkFixedUnliftIoOps' caster ops1 = ops2
 where
  ops2 :: forall eff3
     . (EffConstraint ops2 eff3)
    => FixedUnliftIoOps ops1 res eff1 eff3
  ops2 = FixedUnliftIoOps ops3
   where
    ops3 :: UnliftIoOps (FixedUnliftIoEff ops1 res eff1 ∪ ops1) res eff1 eff3
    ops3 = UnliftIoOps unlift1 $ extractIoResOp ops4

    ops4 :: UnliftIoOps ops1 res eff1 eff3
    ops4 = ops1

    unlift1 :: forall a
        . eff3 (Computation (FixedUnliftIoEff ops1 res eff1 ∪ ops1) (Return a) eff1
              -> IO (res a))
    unlift1 =  do
      unlift2 <- unliftIoOp ops4
      return $ \comp1 ->
        let
          comp2 :: Computation ops1 (Return a) eff1
          comp2 = Computation comp3

          comp3 :: forall eff4 . (Effect eff4)
            => LiftEff eff1 eff4
            -> Operation ops1 eff4
            -> Return a eff4
          comp3 lift13 ops5 = runComp comp1 lift13 (ops6 ∪ ops5)
           where
            ops6 :: FixedUnliftIoOps ops1 res eff1 eff4
            ops6 = withOps ops7 $ ops2

            ops7 :: Operation ops2 eff4
            ops7 = castOps caster ops5
        in
        unlift2 comp2

unfixUnliftIo
  :: forall ops res eff1 eff2 r
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (FixedUnliftIoEff ops res eff1) eff2
     )
  => ((OpsConstraint
        (UnliftIoEff (FixedUnliftIoEff ops res eff1 ∪ ops) res eff1)
        eff2)
      => eff2 r)
  -> eff2 r
unfixUnliftIo comp = withOps (unfixUnliftIoOps captureOps) $ comp

fixedUnliftIo
  :: forall a ops res eff1 eff2
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (FixedUnliftIoEff ops res eff1) eff2
     )
  => (forall eff
       . (EffConstraint (FixedUnliftIoEff ops res eff1 ∪ ops) eff)
      => eff a)
  -> eff2 (IO (res a))
fixedUnliftIo comp = unfixUnliftIo $ unliftIo comp

fixedRunInIo
  :: forall a ops res eff1 eff2
   . ( Effect eff1
     , ImplicitOps ops
     , EffConstraint (FixedUnliftIoEff ops res eff1 ∪ IoEff) eff2
     )
  => (forall eff
       . (EffConstraint (FixedUnliftIoEff ops res eff1 ∪ ops) eff)
      => eff a)
  -> eff2 a
fixedRunInIo comp = unfixUnliftIo $ runInIo comp

ioUnliftIoOps :: FixedUnliftIoOps IoEff Identity IO IO
ioUnliftIoOps = withOps ioOps $ mkFixedUnliftIoOps ops
 where
  ops :: UnliftIoOps IoEff Identity IO IO
  ops = mkUnliftIoOps (return unlift') (return . runIdentity)

  unlift' :: Computation IoEff (Return a) IO -> (IO (Identity a))
  unlift' comp = fmap Identity $ returnVal $ runComp comp idLift ioOps