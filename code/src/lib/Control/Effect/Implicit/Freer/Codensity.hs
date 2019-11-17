module Control.Effect.Implicit.Freer.Codensity
where

import Control.Monad (ap)
import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Freer.CoOp
import Control.Effect.Implicit.Freer.FreeOps
import Control.Effect.Implicit.Freer.FreeEff

newtype Nest f g a = Nest {
  unNest :: f (g a)
} deriving (Functor)

data CoOpCont coop r where
  CoOpCont
    :: forall coop r x
     . coop x
    -> (x -> r)
    -> CoOpCont coop r

newtype Codensity h a = Cod {
  runCodensity
    :: forall x
     . (a -> h x)
    -> h x
}

instance Functor (CoOpCont coop) where
  fmap f (CoOpCont coop cont) =
    CoOpCont coop $ fmap f cont
  {-# INLINE fmap #-}

instance Functor (Codensity h) where
  fmap
    :: forall a b
     . (a -> b)
    -> Codensity h a
    -> Codensity h b
  fmap f (Cod cont1) = Cod cont2
   where
    cont2 :: forall x . (b -> h x) -> h x
    cont2 cont3 = cont1 (\a -> cont3 $ f a)
  {-# INLINE fmap #-}

instance Applicative (Codensity h) where
  pure = return
  (<*>) = ap

instance Monad (Codensity h) where
  return x = Cod $ \cont -> cont x

  (>>=)
    :: forall a b
     . Codensity h a
    -> (a -> Codensity h b)
    -> Codensity h b
  (Cod comp1) >>= cont1 = Cod comp2
   where
    comp2 :: forall x . (b -> h x) -> h x
    comp2 cont2 = comp1 cont3
     where

      cont3 :: a -> h x
      cont3 x = runCodensity (cont1 x) cont2
  {-# INLINE (>>=) #-}

algCod
  :: forall h f a
   . (Functor f)
  => (forall x . f (h x) -> h x)
  -> f (Codensity h a)
  -> Codensity h a
algCod alg op = Cod comp1
 where
  comp1 :: forall x . (a -> h x) -> h x
  comp1 cont1 = alg (fmap cont2 op)
   where
    cont2 :: Codensity h a -> h x
    cont2 (Cod comp2) = comp2 cont1
{-# INLINE algCod #-}

rollCod
  :: forall eff f a
   . (Effect eff)
  => eff (Codensity (Nest eff f) a)
  -> Codensity (Nest eff f) a
rollCod comp1 = Cod comp2
 where
  comp2
    :: forall x
     . (a -> Nest eff f x)
    -> Nest eff f x
  comp2 cont1 = Nest comp3
   where
    comp3 :: eff (f x)
    comp3 = do
      Cod comp4 <- comp1
      unNest $ comp4 cont1
{-# INLINE rollCod #-}

codensityOps
  :: forall ops h
   . ( EffOps ops
     , EffCoOp ops
     , FreeOps ops
     )
  => (forall x r
       . CoOperation ops x
      -> (x -> h r)
      -> h r)
  -> Operation ops (Codensity h)
codensityOps handler1 = mkFreeOps handler2
 where
  handler2 :: forall a . CoOperation ops a -> Codensity h a
  handler2 coop = Cod comp1
   where
    comp1 :: forall x . (a -> h x) -> h x
    comp1 cont = handler1 coop cont
  {-# INLINE handler2 #-}

{-# INLINE codensityOps #-}

toCodensity
  :: forall free ops eff f a
   . ( EffCoOp ops
     , Effect eff
     , FreeOps ops
     , FreeEff free
     )
  => (forall x . CoOpHandler ops x (f x) eff)
  -> free ops eff a
  -> Codensity (Nest eff f) a
toCodensity handler1 comp = toCodensity' (coOpHandler handler1) comp
{-# INLINE toCodensity #-}

toCodensity'
  :: forall free ops eff f a
   . ( EffCoOp ops
     , Effect eff
     , FreeOps ops
     , FreeEff free
     )
  => (forall x r
       . CoOperation ops x
      -> (x -> eff (f r))
      -> eff (f r))
  -> free ops eff a
  -> Codensity (Nest eff f) a
toCodensity' handler1 comp1 = rollCod comp2
 where
  handler2
    :: forall x
     . CoOpCont (CoOperation ops) (Nest eff f x)
    -> Nest eff f x
  handler2 (CoOpCont coop cont1) = Nest $
    handler1 coop (unNest . cont1)

  handler3
    :: forall x
     . CoOpCont
        (CoOperation ops)
        (Codensity (Nest eff f) x)
    -> Codensity (Nest eff f) x
  handler3 = algCod handler2

  handler4
    :: forall x
     . CoOpHandler ops x (Codensity (Nest eff f) x) eff
  handler4 = CoOpHandler (return . return) handler5
   where
    handler5
      :: forall y
       . CoOperation ops y
      -> (y -> eff (Codensity (Nest eff f) x))
      -> eff (Codensity (Nest eff f) x)
    handler5 coop cont1 = return $
      handler3 $ CoOpCont coop $
        fmap rollCod cont1

  comp2 :: eff (Codensity (Nest eff f) a)
  comp2 = handleFree handler4 comp1
{-# INLINE toCodensity' #-}
