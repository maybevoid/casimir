
module Casimir.Freer.Monad.Freer
  ( FreerMonad (..)
  , FreerF (..)
  )
where

import Control.Monad

import Casimir.Base

import Casimir.Freer.CoOp
import Casimir.Freer.FreeOps
import Casimir.Freer.FreeEff

data FreerF ops a b where
  PureF :: a -> FreerF ops a b
  FreeF
    :: forall ops a b x
     . CoOperation ops x
    -> (x -> b)
    -> FreerF ops a b

newtype FreerMonad ops m a = FreerMonad {
  runFreerMonad :: m (FreerF ops a (FreerMonad ops m a))
}

instance
  (Monad m, FreeOps ops)
  => Functor (FreerMonad ops m)
   where
    fmap
      :: forall a b
       . (a -> b)
      -> FreerMonad ops m a
      -> FreerMonad ops m b
    fmap f = mapper1
     where
      mapper1 :: FreerMonad ops m a -> FreerMonad ops m b
      mapper1 (FreerMonad m) = FreerMonad $ fmap mapper2 m

      mapper2
        :: FreerF ops1 a (FreerMonad ops m a)
        -> FreerF ops1 b (FreerMonad ops m b)
      mapper2 (PureF x) = PureF $ f x
      mapper2 (FreeF ops cont) = FreeF ops $ fmap mapper1 cont
    {-# INLINE fmap #-}

instance
  (Monad m, FreeOps ops)
  => Applicative (FreerMonad ops m)
   where
    {-# INLINE pure #-}
    pure a = FreerMonad (return (PureF a))
    (<*>) = ap

instance
  (Monad m, FreeOps ops)
  => Monad (FreerMonad ops m)
   where
    return = pure

    (>>=) :: forall a b
       . FreerMonad ops m a
      -> (a -> FreerMonad ops m b)
      -> FreerMonad ops m b
    FreerMonad m >>= cont1 = FreerMonad $ m >>= cont2
     where
      cont2
        :: FreerF ops a (FreerMonad ops m a)
        -> m (FreerF ops b (FreerMonad ops m b))
      cont2 (PureF x) = runFreerMonad (cont1 x)
      cont2 (FreeF ops cont3)
        = return $ FreeF ops $
            cont3 >=> cont1
      {-# INLINE cont2 #-}
    {-# INLINE (>>=) #-}

instance FreeEff FreerMonad where
  handleFree = doHandleFreer
  {-# INLINE handleFree #-}

  liftFree m = FreerMonad $ fmap PureF m
  {-# INLINE liftFree #-}

  freeOps = mkFreeOps liftFreeOps
  {-# INLINE freeOps #-}

doHandleFreer
  :: forall ops m a r
   . (Monad m, FreeOps ops)
  => CoOpHandler ops a r m
  -> FreerMonad ops m a
  -> m r
doHandleFreer handler = handleFree'
 where
  handleFree'
   :: FreerMonad ops m a
    -> m r
  handleFree' comp = runFreerMonad comp >>= handleComp
  {-# INLINE handleFree' #-}

  handleComp
    :: FreerF ops a (FreerMonad ops m a)
    -> m r
  handleComp (PureF x) = returnHandler handler x
  handleComp (FreeF ops cont)
    = coOpHandler handler ops $
        \x -> handleFree' $ cont x
  {-# INLINE handleComp #-}
{-# INLINE doHandleFreer #-}

liftFreeOps
  :: forall m ops a
   . (Monad m, FreeOps ops)
  => CoOperation ops a
  -> FreerMonad ops m a
liftFreeOps ops =
  FreerMonad $ return $ FreeF ops $
    FreerMonad . return . PureF
{-# INLINE liftFreeOps #-}
