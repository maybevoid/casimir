
module Control.Effect.Implicit.Higher.Free.Church
where

import Control.Monad (ap)

import Data.Kind

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Higher.Free
import Control.Effect.Implicit.Higher.ContraLift

newtype ChurchMonad
  (ops :: (Type -> Type) -> (Type -> Type) -> Type)
  (eff :: Type -> Type)
  a =
    ChurchMonad {
      runChurchMonad
        :: forall t r
         . (Functor (t eff))
        => CoOpHandler ops (t eff)
        -> ContraLift eff (t eff)
        -> (a -> t eff r)
        -> t eff r
    }

instance
  (FreeOps ops)
  => Functor (ChurchMonad ops eff)
  where
    fmap
      :: forall a b
       . (a -> b)
      -> ChurchMonad ops eff a
      -> ChurchMonad ops eff b
    fmap f (ChurchMonad m1) = ChurchMonad m2
     where
      m2
        :: forall t r
         . (Functor (t eff))
        => CoOpHandler ops (t eff)
        -> ContraLift eff (t eff)
        -> (b -> t eff r)
        -> t eff r
      m2 handler contraLift cont =
        m1 handler contraLift $
          cont . f

instance
  (FreeOps ops)
  => Applicative (ChurchMonad ops eff)
  where
    pure
      :: forall a
       . a
      -> ChurchMonad ops eff a
    pure x = ChurchMonad m
     where
      m :: CoOpHandler ops (t eff)
        -> ContraLift eff (t eff)
        -> (a -> t eff r)
        -> t eff r
      m _ _ cont = cont x

    (<*>) = ap

instance
  (FreeOps ops)
  => Monad (ChurchMonad ops eff)
   where
    (>>=)
      :: forall a b
       . ChurchMonad ops eff a
      -> (a -> ChurchMonad ops eff b)
      -> ChurchMonad ops eff b
    (ChurchMonad m1) >>= cont1 = ChurchMonad m2
     where
      m2
        :: forall t r
         . (Functor (t eff))
        => CoOpHandler ops (t eff)
        -> ContraLift eff (t eff)
        -> (b -> t eff r)
        -> t eff r
      m2 coopHandler contraLift cont2 =
        m1 coopHandler contraLift cont3
         where
          cont3 :: a -> t eff r
          cont3 x =
            let (ChurchMonad m3) = cont1 x in
            m3 coopHandler contraLift cont2

contraLiftFree
  :: forall ops eff
   . (FreeOps ops)
  => ContraLift eff (ChurchMonad ops eff)
contraLiftFree = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . (forall w
         . (Functor w)
        => (forall x . ChurchMonad ops eff x -> eff (w x))
        -> eff (w a))
    -> ChurchMonad ops eff a
  contraLift1 cont1 = ChurchMonad m1
   where
    m1
      :: forall t r
       . (Functor (t eff))
      => CoOpHandler ops (t eff)
      -> ContraLift eff (t eff)
      -> (a -> t eff r)
      -> t eff r
    m1 coopHandler contraLift2 cont =
      joinHandler coopHandler res2
     where
      res2 :: t eff (t eff r)
      res2 = fmap cont res1

      res1 :: t eff a
      res1 = runContraLift contraLift2 cont2

      cont2
        :: forall w
         . (Functor w)
        => (forall x . t eff x -> eff (w x))
        -> eff (w a)
      cont2 contraLift3 = cont1 contraLift4
       where
        contraLift4
          :: forall x
            . ChurchMonad ops eff x
          -> eff (w x)
        contraLift4 (ChurchMonad m3) = contraLift3 m4
         where
          m4 :: t eff x
          m4 = m3 coopHandler contraLift2 $
            returnHandler coopHandler
