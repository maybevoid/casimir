module Casimir.Higher.Monad.Free
where

import Data.Kind
import Control.Monad (ap)

import Casimir.Base
import Casimir.Higher.Free

data FreeF
  ops
  (m1 :: Type -> Type)
  (m2 :: Type -> Type)
  a
  where
    PureF :: a -> FreeF ops m1 m2 a

    BindF
      :: forall ops m1 m2 a b
       . CoOperation ops m2 a
      -> (a -> m2 b)
      -> FreeF ops m1 m2 b

    ContraF
      :: forall ops m1 m2 a b
       . (forall w
           . (Functor w)
          => (forall x . m2 x -> m1 (w x))
          -> m1 (w a))
      -> (a -> m2 b)
      -> FreeF ops m1 m2 b

newtype FreeMonad ops m a = FreeMonad {
  runFreerMonad :: m (FreeF ops m (FreeMonad ops m) a)
}

liftPure
  :: forall ops m a
   . ( Monad m
     , FreeOps ops)
  => a
  -> FreeMonad ops m a
liftPure = FreeMonad . return . PureF

instance
  ( Monad m
  , FreeOps ops)
  => Functor (FreeMonad ops m)
   where
    fmap
      :: forall a b
       . (a -> b)
      -> FreeMonad ops m a
      -> FreeMonad ops m b
    fmap f = mapper1
     where
      mapper1 :: FreeMonad ops m a -> FreeMonad ops m b
      mapper1 (FreeMonad m) = FreeMonad $ fmap mapper2 m

      mapper2
        :: FreeF ops m (FreeMonad ops m) a
        -> FreeF ops m (FreeMonad ops m) b
      mapper2 (PureF x) = PureF $ f x
      mapper2 (BindF coop cont) = BindF coop $ fmap (fmap f) cont

      mapper2 (ContraF cont1 cont2) = ContraF cont1 $ fmap (fmap f) cont2

instance
  ( Monad m
  , FreeOps ops
  )
  => Applicative (FreeMonad ops m)
   where
    pure = liftPure
    (<*>) = ap

instance
  ( Monad m
  , FreeOps ops
  )
  => Monad (FreeMonad ops m)
   where
    return = liftPure

    (>>=)
      :: forall a b
       . FreeMonad ops m a
      -> (a -> FreeMonad ops m b)
      -> FreeMonad ops m b
    (FreeMonad m1) >>= cont1 = FreeMonad m2
     where
      m2 :: m (FreeF ops m (FreeMonad ops m) b)
      m2 = do
        m3 <- m1
        case m3 of
          PureF a ->
            let (FreeMonad res) = cont1 a in
            res

          BindF coop cont2 ->
            return $ BindF coop $ \x ->
              cont2 x >>= cont1

          ContraF cont2 cont3 ->
            return $ ContraF cont2 $ \x ->
              cont3 x >>= cont1

contraLiftFree
  :: forall ops m
   . (FreeOps ops, Monad m)
  => ContraLift m (FreeMonad ops m)
contraLiftFree = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . (forall w
         . (Functor w)
        => (forall x . FreeMonad ops m x -> m (w x))
        -> m (w a))
    -> FreeMonad ops m a
  contraLift1 cont1 = FreeMonad $ return $
    ContraF cont2 return
     where
      cont2
        :: forall w
         . (Functor w)
        => (forall x . FreeMonad ops m x -> m (w x))
        -> m (w a)
      cont2 contraLift2 = cont1 contraLift2
