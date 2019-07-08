module Control.Effect.Implicit.Ops.UnliftIo
where

import Data.Kind
import Control.Monad.Identity

import Control.Effect.Implicit.Cast
import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Free
import Control.Effect.Implicit.Ops.Io

newtype GenericComp ops a = GenericComp {
  unGenericComp
    :: forall eff
     . (EffConstraint ops eff)
    => eff a
}

data UnliftIoEff
  (res :: Type -> Type)
  (ops :: Type)

data FixedUnliftIoEff
  (res :: Type -> Type)
  (ops :: Type)

data UnliftIoOps res ops eff = UnliftIoOps {
  unliftIoOp
    :: forall a
     . eff
        (GenericComp ops a
         -> IO (res a))
  ,
  extractIoResOp :: forall a . res a -> eff a
}

data UnliftIoCoOp res ops r where
  UnliftIoOp
    :: forall res ops r a
     . ((GenericComp ops a -> IO (res a))
        -> r)
     -> UnliftIoCoOp res ops r

  ExtractIoResOp
    :: forall res ops r a
     . res a
    -> (a -> r)
    -> UnliftIoCoOp res ops r

data FixedUnliftIoOps res ops eff = FixedUnliftIoOps {
  unfixUnliftIoOps
    :: UnliftIoOps res (FixedUnliftIoEff res ops ∪ ops) eff
}

instance EffOps (UnliftIoEff res ops) where
  type Operation (UnliftIoEff res ops) = UnliftIoOps res ops

instance EffCoOp (UnliftIoEff res ops) where
  type CoOperation (UnliftIoEff res ops) = UnliftIoCoOp res ops

instance EffOps (FixedUnliftIoEff res ops) where
  type Operation (FixedUnliftIoEff res ops) =
    FixedUnliftIoOps res ops

instance Functor (UnliftIoCoOp res ops) where
  fmap f (UnliftIoOp cont) = UnliftIoOp $ fmap f cont
  fmap f (ExtractIoResOp res cont) = ExtractIoResOp res $ fmap f cont

instance EffFunctor (UnliftIoOps res ops) where
  effmap lift ops = UnliftIoOps {
    unliftIoOp = lift $ unliftIoOp ops,
    extractIoResOp = \res -> lift $ extractIoResOp ops res
  }

instance EffFunctor (FixedUnliftIoOps res ops) where
  effmap lift (FixedUnliftIoOps ops) =
    FixedUnliftIoOps $ effmap lift ops

instance FreeOps (UnliftIoEff res ops) where
  mkFreeOps liftCoOp = UnliftIoOps {
    unliftIoOp = liftCoOp $ UnliftIoOp id,
    extractIoResOp = \res -> liftCoOp $ ExtractIoResOp res id
  }

instance ImplicitOps (UnliftIoEff res ops) where
  type OpsConstraint (UnliftIoEff res ops) eff =
    (?_Control_Effect_Implicit_Ops_UnliftIo_unliftIoOps
      :: UnliftIoOps res ops eff)

  withOps ops comp =
    let
      ?_Control_Effect_Implicit_Ops_UnliftIo_unliftIoOps =
        ops in comp

  captureOps =
    ?_Control_Effect_Implicit_Ops_UnliftIo_unliftIoOps

instance ImplicitOps (FixedUnliftIoEff res ops) where
  type OpsConstraint (FixedUnliftIoEff res ops) eff =
    (?_Control_Effect_Implicit_Ops_UnliftIo_fixedUnliftIoOps
      :: FixedUnliftIoOps res ops eff)

  withOps ops comp =
    let
      ?_Control_Effect_Implicit_Ops_UnliftIo_fixedUnliftIoOps =
        ops in comp

  captureOps =
    ?_Control_Effect_Implicit_Ops_UnliftIo_fixedUnliftIoOps

unliftIo
  :: forall eff a res ops
   . ( ImplicitOps ops
     , EffConstraint (UnliftIoEff res ops) eff
     )
  => (forall eff2 . (EffConstraint ops eff2) => eff2 a)
  -> eff (IO (res a))
unliftIo comp1 = unliftIoOp captureOps <*> pure comp2
 where
  comp2 :: GenericComp ops a
  comp2 = GenericComp comp1

extractIoRes
  :: forall a res ops eff
   . ( EffConstraint (UnliftIoEff res ops) eff
     )
  => res a
  -> eff a
extractIoRes = extractIoResOp captureOps

runInIo
  :: forall res ops eff a
   . ( ImplicitOps ops
     , EffConstraint (UnliftIoEff res ops ∪ IoEff) eff
     )
  => (forall eff2 . (EffConstraint ops eff2) => eff2 a)
  -> eff a
runInIo comp = do
  action <- unliftIo comp
  res <- liftIo action
  extractIoRes res

mkUnliftIoOps
  :: forall res ops eff
   . ( Effect eff
     , ImplicitOps ops
     )
  => (forall a . eff (GenericComp ops a -> (IO (res a))))
  -> (forall a . res a -> eff a)
  -> UnliftIoOps res ops eff
mkUnliftIoOps = UnliftIoOps

mkFixedUnliftIoOps'
  :: forall ops2 ops1 res eff
    . ( ImplicitOps ops1
      , ImplicitOps ops2
      , EffConstraint ops2 eff
      )
  => OpsCast ops1 ops2
  -> (forall eff2
      . (EffConstraint ops2 eff2)
      => UnliftIoOps res ops1 eff2)
  -> FixedUnliftIoOps res ops1 eff
mkFixedUnliftIoOps' caster ops1 = ops2
 where
  ops2 :: forall eff2
     . (EffConstraint ops2 eff2)
    => FixedUnliftIoOps res ops1 eff2
  ops2 = FixedUnliftIoOps $ UnliftIoOps unlift1 $ extractIoResOp ops1
   where
    unlift1
      :: forall a
       . eff2
          (GenericComp (FixedUnliftIoEff res ops1 ∪ ops1) a
            -> IO (res a))
    unlift1 =  do
      unlift2 <- unliftIoOp ops1
      return $ \(GenericComp comp1) ->
        let
          comp2
            :: forall eff3
             . (EffConstraint ops1 eff3)
            => eff3 a
          comp2 = withOps ops4 comp1
           where
            ops4 :: FixedUnliftIoOps res ops1 eff3
            ops4 = withCast @eff3 @ops1 @ops2 caster ops2
        in
        unlift2 $ GenericComp comp2

mkFixedUnliftIoOps
  :: forall ops res eff
   . ( ImplicitOps ops
     , EffConstraint ops eff
     )
  => (forall eff2
      . (EffConstraint ops eff2)
      => UnliftIoOps res ops eff2)
  -> FixedUnliftIoOps res ops eff
mkFixedUnliftIoOps = mkFixedUnliftIoOps' @ops cast

unfixUnliftIo
  :: forall res ops eff r
   . ( ImplicitOps ops
     , EffConstraint (FixedUnliftIoEff res ops) eff
     )
  => ((OpsConstraint
        (UnliftIoEff res (FixedUnliftIoEff res ops ∪ ops))
        eff)
      => eff r)
  -> eff r
unfixUnliftIo comp = withOps (unfixUnliftIoOps captureOps) $ comp

fixedUnliftIo
  :: forall a res ops eff
   . ( ImplicitOps ops
     , EffConstraint (FixedUnliftIoEff res ops) eff
     )
  => (forall eff2
       . (EffConstraint (FixedUnliftIoEff res ops ∪ ops) eff2)
      => eff2 a)
  -> eff (IO (res a))
fixedUnliftIo comp = unfixUnliftIo $ unliftIo comp

fixedRunInIo
  :: forall a res ops eff
   . ( Effect eff
     , ImplicitOps ops
     , EffConstraint (FixedUnliftIoEff res ops ∪ IoEff) eff
     )
  => (forall eff2
       . (EffConstraint (FixedUnliftIoEff res ops ∪ ops) eff2)
      => eff2 a)
  -> eff a
fixedRunInIo comp = unfixUnliftIo $ runInIo comp

ioUnliftIoOps
  :: forall eff
   . (Effect eff)
  => FixedUnliftIoOps Identity IoEff eff
ioUnliftIoOps = mkFixedUnliftIoOps' @NoEff @IoEff cast ops
 where
  ops
    :: forall eff2
     . (Effect eff2)
    => UnliftIoOps Identity IoEff eff2
  ops = mkUnliftIoOps (return unlift') (return . runIdentity)

  unlift' :: GenericComp IoEff a -> (IO (Identity a))
  unlift' (GenericComp comp) =
    fmap Identity $ withOps ioOps comp
