
module Casimir.Higher.Monad.Church
where

import Control.Monad (ap)

import Data.Kind

import Casimir.Base (ContraLift (..))
import Casimir.Higher.Base
import Casimir.Higher.Free

newtype ChurchMonad
  ops
  (m :: Type -> Type)
  a =
    ChurchMonad {
      runChurchMonad
        :: forall f r
         . (Functor f)
        => CoOpHandler ops f m
        -> (a -> m (f r))
        -> m (f r)
    }

instance
  (FreeOps ops, Functor m)
  => Functor (ChurchMonad ops m)
  where
    fmap
      :: forall a b
       . (a -> b)
      -> ChurchMonad ops m a
      -> ChurchMonad ops m b
    fmap f (ChurchMonad m1) = ChurchMonad m2
     where
      m2
        :: forall f r
         . (Functor f)
        => CoOpHandler ops f m
        -> (b -> m (f r))
        -> m (f r)
      m2 handler cont =
        m1 handler $ cont . f

instance
  (FreeOps ops, Monad m)
  => Applicative (ChurchMonad ops m)
  where
    pure
      :: forall a
       . a
      -> ChurchMonad ops m a
    pure x = ChurchMonad m
     where
      m :: forall f r
         . (Functor f)
        => CoOpHandler ops f m
        -> (a -> m (f r))
        -> m (f r)
      m _ cont = cont x

    (<*>) = ap

instance
  (FreeOps ops, Monad m)
  => Monad (ChurchMonad ops m)
   where
    (>>=)
      :: forall a b
       . ChurchMonad ops m a
      -> (a -> ChurchMonad ops m b)
      -> ChurchMonad ops m b
    (ChurchMonad m1) >>= cont1 = ChurchMonad m2
     where
      m2
        :: forall f r
         . (Functor f)
        => CoOpHandler ops f m
        -> (b -> m (f r))
        -> m (f r)
      m2 coopHandler cont2 =
        m1 coopHandler cont3
       where
        cont3 :: a -> m (f r)
        cont3 a = do
          let (ChurchMonad m3) = cont1 a
          m3 coopHandler cont2

instance FreeEff ChurchMonad where
  freeOps
    :: forall ops m
     . (FreeOps ops, Monad m)
    => Operation ops (ChurchMonad ops m) (ChurchMonad ops m)
  freeOps = mkFreeOps liftOps
   where
    liftOps
      :: forall a
       . CoOperation ops (ChurchMonad ops m) a
      -> ChurchMonad ops m a
    liftOps coop1 = ChurchMonad m1
     where
      m1
        :: forall f r
         . (Functor f)
        => CoOpHandler ops f m
        -> (a -> m (f r))
        -> m (f r)
      m1 coopHandler cont =
        operationHandler coopHandler coop2 cont
       where
        coop2 :: CoOperation ops (m ∘ f) a
        coop2 = liftCoOp runChurch coop1

        runChurch
          :: forall x
           . ChurchMonad ops m x
          -> (m ∘ f) x
        runChurch (ChurchMonad m2) = Nest $
          m2 coopHandler $ returnHandler coopHandler

  liftFree
    :: forall ops m a
     . (FreeOps ops, Monad m)
    => m a
    -> ChurchMonad ops m a
  liftFree m1 = ChurchMonad m2
   where
    m2
      :: forall f r
        . (Functor f)
      => CoOpHandler ops f m
      -> (a -> m (f r))
      -> m (f r)
    m2 _ cont = m1 >>= cont

  freeContraLift
    :: forall ops m
    . (FreeOps ops, Monad m)
    => ContraLift m (ChurchMonad ops m)
  freeContraLift = ContraLift contraLift1
   where
    contraLift1
      :: forall a
      . (forall w
          . (Functor w)
          => (forall x . ChurchMonad ops m x -> m (w x))
          -> m (w a))
      -> ChurchMonad ops m a
    contraLift1 cont1 = ChurchMonad m1
     where
      m1
        :: forall f r
          . (Functor f)
        => CoOpHandler ops f m
        -> (a -> m (f r))
        -> m (f r)
      m1 coopHandler cont2 =
        contraLiftHandler coopHandler cont3
       where
        cont3
          :: forall w
           . (Functor w)
          => (forall x . f (m x) -> m (w x))
          -> m (w (m (f r)))
        cont3 contraFree2 = do
          wa :: w a <- cont1 contraLift2
          return $ fmap cont2 wa
         where
          contraLift2
            :: forall x
             . ChurchMonad ops m x
            -> m (w x)
          contraLift2 (ChurchMonad m2) = do
            fx <- m2 coopHandler $ returnHandler coopHandler
            contraFree2 $ fmap return fx

instance FreeHandler ChurchMonad where
  handleFree
    :: forall ops m f a
     . ( Monad m
       , FreeOps ops
       , Functor f
       )
    => CoOpHandler ops f m
    -> ChurchMonad ops m a
    -> m (f a)
  handleFree coopHandler (ChurchMonad m1) =
    m1 coopHandler $ returnHandler coopHandler
