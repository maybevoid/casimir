
module Control.Effect.Implicit.Freer.Freer
  ( FreerMonad (..)
  , FreerF (..)
  )
where

import Control.Monad

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Freer.FreerEff

data FreerF ops a b where
  PureF
    :: forall ops a b
     . a -> FreerF ops a b
  FreeF
    :: forall ops a b x
     . FreerCoOp ops x
    -> (x -> b)
    -> FreerF ops a b

newtype FreerMonad ops eff a = FreerMonad {
  runFreerMonad :: eff (FreerF ops a (FreerMonad ops eff a))
}

instance
  (Monad eff, FreerOps ops)
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
  (Monad eff, FreerOps ops)
  => Applicative (FreerMonad ops eff)
   where
    {-# INLINE pure #-}
    pure a = FreerMonad (return (PureF a))
    (<*>) = ap

instance
  (Monad eff, FreerOps ops)
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

instance FreerEff FreerMonad where
  handleFreer = doHandleFreer
  {-# INLINE handleFreer #-}

  liftFreer m = FreerMonad $ fmap PureF m
  {-# INLINE liftFreer #-}

  freerOps = mkFreerOps liftFreerOps
  {-# INLINE freerOps #-}

doHandleFreer
  :: forall ops eff a r
   . (Effect eff, FreerOps ops)
  => FreerCoOpHandler ops a r eff
  -> FreerMonad ops eff a
  -> eff r
doHandleFreer handler = handleFreer'
 where
  handleFreer'
   :: FreerMonad ops eff a
    -> eff r
  handleFreer' comp = runFreerMonad comp >>= handleComp
  {-# INLINE handleFreer' #-}

  handleComp
    :: FreerF ops a (FreerMonad ops eff a)
    -> eff r
  handleComp (PureF x) = handleFreerReturn handler x
  handleComp (FreeF ops cont)
    = handleFreerCoOp handler ops $
        \x -> handleFreer' $ cont x
  {-# INLINE handleComp #-}
{-# INLINE doHandleFreer #-}

liftFreerOps
  :: forall eff ops a
   . (Effect eff, FreerOps ops)
  => FreerCoOp ops a
  -> FreerMonad ops eff a
liftFreerOps ops =
  FreerMonad $ return $ FreeF ops $
    FreerMonad . return . PureF
{-# INLINE liftFreerOps #-}
