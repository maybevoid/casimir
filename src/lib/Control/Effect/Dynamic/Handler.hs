
module Control.Effect.Dynamic.Handler
where

import Control.Monad.Identity

import Control.Effect.Base
import Control.Effect.Computation
import Control.Effect.Dynamic.Class
import Control.Effect.Dynamic.Lift

mkDynamicArrow
  :: forall a b ops handler eff1 .
  ( Effect eff1
  , EffOps ops
  , EffOps handler
  , DynamicOps handler
  )
  => (forall eff2 .
      (Effect eff2, OpsConstraint ops eff2)
      => OpsHandler handler a b eff2)
  -> ArrowComputation ops handler a b eff1
mkDynamicArrow handler1 = Computation comp1
 where
  comp1
    :: forall eff2 .
    (Effect eff2)
    => LiftEff eff1 eff2
    -> Operation ops eff2
    -> Arrow handler a b eff2
  comp1 _ ops1 = Arrow arrow1
   where
    arrow1 :: Computation handler (Return a) eff2 -> eff2 b
    arrow1 comp2 = runDynamicEff comp3 handler2
     where
      comp3 :: DynamicEff handler eff2 a
      comp3 = returnVal $ runComp comp2 liftDynamicEff dynamicOps

      handler2 :: OpsHandler handler a b eff2
      handler2 = bindConstraint ops1 handler1

withOpsHandler
  :: forall ops eff a b .
  ( Effect eff
  , EffOps ops
  , DynamicOps ops
  )
  => OpsHandler ops a b eff
  -> (OpsConstraint ops (DynamicEff ops eff)
      => DynamicEff ops eff a)
  -> eff b
withOpsHandler handler comp1 = runDynamicEff comp2 handler
 where
  ops :: Operation ops (DynamicEff ops eff)
  ops = dynamicOps

  comp2 :: DynamicEff ops eff a
  comp2 = bindConstraint ops comp1

identityOpsHandler
  :: forall handler eff .
  (Effect eff, EffOps handler)
  => (forall a . OpsHandler handler a a eff)
  -> GenericOpsHandler Identity handler eff
identityOpsHandler handler1 = GenericOpsHandler $ handler2
 where
  handler2 :: forall a . OpsHandler handler a (Identity a) eff
  handler2 = OpsHandler {
    handleReturn = wrapReturn,
    handleOps = wrapOps
  }
   where
    wrapReturn :: a -> eff (Identity a)
    wrapReturn x = fmap Identity $ handleReturn handler1 x

    wrapOps :: CoOperation handler (eff (Identity a)) -> eff (Identity a)
    wrapOps ops = fmap Identity $ handleOps handler1 $
      (fmap . fmap) runIdentity ops