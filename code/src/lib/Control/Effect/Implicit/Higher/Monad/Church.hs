
module Control.Effect.Implicit.Higher.Monad.Church
where

import Control.Monad (ap)

import Data.Kind

import Control.Effect.Implicit.Higher.Base
import Control.Effect.Implicit.Higher.Free
import Control.Effect.Implicit.Higher.CoOp
import Control.Effect.Implicit.Higher.ContraLift

newtype ChurchMonad
  ops
  (eff :: Type -> Type)
  a =
    ChurchMonad {
      runChurchMonad
        :: forall f r
         . (Functor f)
        => CoOpHandler ops eff f
        -> (a -> eff (f r))
        -> eff (f r)
    }

instance
  (FreeOps ops, Functor eff)
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
        :: forall f r
         . (Functor f)
        => CoOpHandler ops eff f
        -> (b -> eff (f r))
        -> eff (f r)
      m2 handler cont =
        m1 handler $ cont . f

instance
  (FreeOps ops, Effect eff)
  => Applicative (ChurchMonad ops eff)
  where
    pure
      :: forall a
       . a
      -> ChurchMonad ops eff a
    pure x = ChurchMonad m
     where
      m :: forall f r
         . (Functor f)
        => CoOpHandler ops eff f
        -> (a -> eff (f r))
        -> eff (f r)
      m _ cont = cont x

    (<*>) = ap

instance
  (FreeOps ops, Effect eff)
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
        :: forall f r
         . (Functor f)
        => CoOpHandler ops eff f
        -> (b -> eff (f r))
        -> eff (f r)
      m2 coopHandler cont2 =
        m1 coopHandler cont3
       where
        cont3 :: a -> eff (f r)
        cont3 a = do
          let (ChurchMonad m3) = cont1 a
          m3 coopHandler cont2

instance FreeEff ChurchMonad where
  freeOps
    :: forall ops eff
     . (FreeOps ops, Effect eff)
    => Operation ops (ChurchMonad ops eff) (ChurchMonad ops eff)
  freeOps = mkFreeOps liftOps
   where
    liftOps
      :: forall a
       . CoOperation ops (ChurchMonad ops eff) a
      -> ChurchMonad ops eff a
    liftOps coop1 = ChurchMonad m1
     where
      m1
        :: forall f r
         . (Functor f)
        => CoOpHandler ops eff f
        -> (a -> eff (f r))
        -> eff (f r)
      m1 coopHandler cont =
        operationHandler coopHandler coop2 cont
       where
        coop2 :: CoOperation ops (eff ∘ f) a
        coop2 = liftCoOp runChurch coop1

        runChurch
          :: forall x
           . ChurchMonad ops eff x
          -> (eff ∘ f) x
        runChurch (ChurchMonad m2) = Nest $
          m2 coopHandler $ returnHandler coopHandler

  liftFree
    :: forall ops eff a
     . (FreeOps ops, Effect eff)
    => eff a
    -> ChurchMonad ops eff a
  liftFree m1 = ChurchMonad m2
   where
    m2
      :: forall f r
        . (Functor f)
      => CoOpHandler ops eff f
      -> (a -> eff (f r))
      -> eff (f r)
    m2 _ cont = m1 >>= cont

instance FreeHandler ChurchMonad where
  handleFree
    :: forall ops eff f a
     . ( Effect eff
       , FreeOps ops
       , Functor f
       )
    => CoOpHandler ops eff f
    -> ChurchMonad ops eff a
    -> eff (f a)
  handleFree coopHandler (ChurchMonad m1) =
    m1 coopHandler $ returnHandler coopHandler

  freeContraLift
    :: forall ops eff
    . (FreeOps ops, Effect eff)
    => ContraLift eff (ChurchMonad ops eff)
  freeContraLift = ContraLift contraLift1
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
        :: forall f r
          . (Functor f)
        => CoOpHandler ops eff f
        -> (a -> eff (f r))
        -> eff (f r)
      m1 coopHandler cont2 =
        contraLiftHandler coopHandler cont3
       where
        cont3
          :: forall w
           . (Functor w)
          => (forall x . f (eff x) -> eff (w x))
          -> eff (w (eff (f r)))
        cont3 contraFree2 = do
          wa :: w a <- cont1 contraLift2
          return $ fmap cont2 wa
         where
          contraLift2
            :: forall x
             . ChurchMonad ops eff x
            -> eff (w x)
          contraLift2 (ChurchMonad m2) = do
            fx <- m2 coopHandler $ returnHandler coopHandler
            contraFree2 $ fmap return fx
