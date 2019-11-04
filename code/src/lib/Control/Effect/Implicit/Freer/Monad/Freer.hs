
module Control.Effect.Implicit.Freer.Monad.Freer
  ( FreerMonad (..)
  , FreerF (..)
  )
where

import Control.Monad

import Control.Effect.Implicit.Base

import Control.Effect.Implicit.Freer.CoOp
import Control.Effect.Implicit.Freer.FreeOps
import Control.Effect.Implicit.Freer.FreeEff

data FreerF ops a b where
  PureF :: a -> FreerF ops a b
  FreeF
    :: forall ops a b x
     . CoOperation ops x
    -> (x -> b)
    -> FreerF ops a b

newtype FreerMonad ops eff a = FreerMonad {
  runFreerMonad :: eff (FreerF ops a (FreerMonad ops eff a))
}

instance
  (Monad eff, FreeOps ops)
  => Functor (FreerMonad ops eff)
   where
    fmap
      :: forall a b
       . (a -> b)
      -> FreerMonad ops eff a
      -> FreerMonad ops eff b
    fmap f = mapper1
     where
      mapper1 :: FreerMonad ops eff a -> FreerMonad ops eff b
      mapper1 (FreerMonad m) = FreerMonad $ fmap mapper2 m

      mapper2
        :: FreerF ops1 a (FreerMonad ops eff a)
        -> FreerF ops1 b (FreerMonad ops eff b)
      mapper2 (PureF x) = PureF $ f x
      mapper2 (FreeF ops cont) = FreeF ops $ fmap mapper1 cont
    {-# INLINE fmap #-}

instance
  (Monad eff, FreeOps ops)
  => Applicative (FreerMonad ops eff)
   where
    {-# INLINE pure #-}
    pure a = FreerMonad (return (PureF a))
    (<*>) = ap

instance
  (Monad eff, FreeOps ops)
  => Monad (FreerMonad ops eff)
   where
    return = pure

    (>>=) :: forall a b
       . FreerMonad ops eff a
      -> (a -> FreerMonad ops eff b)
      -> FreerMonad ops eff b
    FreerMonad m >>= cont1 = FreerMonad $ m >>= cont2
     where
      cont2
        :: FreerF ops a (FreerMonad ops eff a)
        -> eff (FreerF ops b (FreerMonad ops eff b))
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
  :: forall ops eff a r
   . (Effect eff, FreeOps ops)
  => CoOpHandler ops a r eff
  -> FreerMonad ops eff a
  -> eff r
doHandleFreer handler = handleFree'
 where
  handleFree'
   :: FreerMonad ops eff a
    -> eff r
  handleFree' comp = runFreerMonad comp >>= handleComp
  {-# INLINE handleFree' #-}

  handleComp
    :: FreerF ops a (FreerMonad ops eff a)
    -> eff r
  handleComp (PureF x) = returnHandler handler x
  handleComp (FreeF ops cont)
    = coOpHandler handler ops $
        \x -> handleFree' $ cont x
  {-# INLINE handleComp #-}
{-# INLINE doHandleFreer #-}

liftFreeOps
  :: forall eff ops a
   . (Effect eff, FreeOps ops)
  => CoOperation ops a
  -> FreerMonad ops eff a
liftFreeOps ops =
  FreerMonad $ return $ FreeF ops $
    FreerMonad . return . PureF
{-# INLINE liftFreeOps #-}
