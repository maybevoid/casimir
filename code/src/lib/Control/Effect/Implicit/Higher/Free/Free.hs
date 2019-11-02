module Control.Effect.Implicit.Higher.Free.Free
where

import Data.Kind
import Control.Monad (ap)

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.Free
import Control.Effect.Implicit.Higher.ContraLift

data FreeF
  (ops :: (Type -> Type) -> (Type -> Type) -> Type)
  (eff1 :: Type -> Type)
  (eff2 :: Type -> Type)
  a
  where
    PureF :: a -> FreeF ops eff1 eff2 a

    BindF
      :: forall ops eff1 eff2 a b
       . CoOperation ops eff2 a
      -> (a -> eff2 b)
      -> FreeF ops eff1 eff2 b

    ContraF
      :: forall ops eff1 eff2 a
       . (forall w
           . (Functor w)
          => (forall x . eff2 x -> eff1 (w x))
          -> eff1 (w (eff2 a)))
      -> FreeF ops eff1 eff2 a

newtype FreeMonad ops eff a = FreeMonad {
  runFreerMonad :: eff (FreeF ops eff (FreeMonad ops eff) a)
}

liftPure
  :: forall ops eff a
   . ( Monad eff
     , FreeOps ops)
  => a
  -> FreeMonad ops eff a
liftPure = FreeMonad . return . PureF

instance
  ( Monad eff
  , FreeOps ops)
  => Functor (FreeMonad ops eff)
   where
    fmap
      :: forall a b
       . (a -> b)
      -> FreeMonad ops eff a
      -> FreeMonad ops eff b
    fmap f = mapper1
     where
      mapper1 :: FreeMonad ops eff a -> FreeMonad ops eff b
      mapper1 (FreeMonad m) = FreeMonad $ fmap mapper2 m

      mapper2
        :: FreeF ops eff (FreeMonad ops eff) a
        -> FreeF ops eff (FreeMonad ops eff) b
      mapper2 (PureF x) = PureF $ f x
      mapper2 (BindF coop cont) = BindF coop $ fmap (fmap f) cont

      mapper2 (ContraF cont1) = ContraF $ cont2
       where
        cont2
          :: forall w
           . (Functor w)
          => (forall x . FreeMonad ops eff x -> eff (w x))
          -> eff (w (FreeMonad ops eff b))
        cont2 contraLift =
          fmap (fmap (fmap f)) $ cont1 contraLift

instance
  ( Monad eff
  , FreeOps ops
  )
  => Applicative (FreeMonad ops eff)
   where
    pure = liftPure
    (<*>) = ap

instance
  ( Monad eff
  , FreeOps ops
  )
  => Monad (FreeMonad ops eff)
   where
    return = liftPure

    (>>=)
      :: forall a b
       . FreeMonad ops eff a
      -> (a -> FreeMonad ops eff b)
      -> FreeMonad ops eff b
    (FreeMonad m1) >>= cont1 = FreeMonad m2
     where
      m2 :: eff (FreeF ops eff (FreeMonad ops eff) b)
      m2 = do
        m3 <- m1
        case m3 of
          PureF a ->
            let (FreeMonad res) = cont1 a in
            res

          BindF coop cont2 ->
            return $ BindF coop $ \x ->
              cont2 x >>= cont1

          ContraF cont2 ->
            return $ ContraF cont3
            where
              cont3
                :: forall w
                 . (Functor w)
                => (forall x . FreeMonad ops eff x -> eff (w x))
                -> eff (w (FreeMonad ops eff b))
              cont3 contraLift =
                fmap
                  (fmap $ \mx -> mx >>= cont1) $
                  cont2 contraLift

contraLiftFree
  :: forall ops eff
   . (FreeOps ops, Effect eff)
  => ContraLift eff (FreeMonad ops eff)
contraLiftFree = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . (forall w
         . (Functor w)
        => (forall x . FreeMonad ops eff x -> eff (w x))
        -> eff (w a))
    -> FreeMonad ops eff a
  contraLift1 cont1 = FreeMonad $ return $
    ContraF cont2
     where
      cont2
        :: forall w
         . (Functor w)
        => (forall x . FreeMonad ops eff x -> eff (w x))
        -> eff (w (FreeMonad ops eff a))
      cont2 contraLift2 = fmap (fmap return) $ cont1 contraLift2
