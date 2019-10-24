
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
        :: forall f r
         . (Functor f)
        => CoOpHandler ops eff f
        -> ContraFree eff f
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
        -> ContraFree eff f
        -> (b -> eff (f r))
        -> eff (f r)
      m2 handler contraFree cont =
        m1 handler contraFree $ cont . f

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
        -> ContraFree eff f
        -> (a -> eff (f r))
        -> eff (f r)
      m _ _ cont = cont x

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
        -> ContraFree eff f
        -> (b -> eff (f r))
        -> eff (f r)
      m2 coopHandler contraFree cont2 =
        m1 coopHandler contraFree cont3
       where
        cont3 :: a -> eff (f r)
        cont3 a = do
          let (ChurchMonad m3) = cont1 a
          m3 coopHandler contraFree cont2

instance FreeEff ChurchMonad where
  freeOps
    :: forall ops eff
     . (FreeOps ops, Effect eff)
    => ops (ChurchMonad ops eff) (ChurchMonad ops eff)
  freeOps = undefined

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
      -> ContraFree eff f
      -> (a -> eff (f r))
      -> eff (f r)
    m2 _ _ cont = m1 >>= cont

instance FreeHandler ChurchMonad where
  handleFree
    :: forall ops eff f a
     . ( Effect eff
       , FreeOps ops
       , Functor f
       )
    => CoOpHandler ops eff f
    -> ContraFree eff f
    -> ChurchMonad ops eff a
    -> eff (f a)
  handleFree coopHandler contraFree (ChurchMonad m1) =
    m1 coopHandler contraFree $ returnHandler coopHandler

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
        -> ContraFree eff f
        -> (a -> eff (f r))
        -> eff (f r)
      m1 coopHandler contraFree1 cont2 =
        runContraFree contraFree1 cont3 cont2
       where
        cont3
          :: forall w
           . (Functor w)
          => (forall x . f x -> eff (w x))
          -> eff (w a)
        cont3 contraFree2 = cont1 contraLift2
         where
          contraLift2
            :: forall x
             . ChurchMonad ops eff x
            -> eff (w x)
          contraLift2 (ChurchMonad m2) = do
            fx <- m2 coopHandler contraFree1 $ returnHandler coopHandler
            contraFree2 fx

--      where
--       res2 :: f (f r)
--       res2 = fmap cont2 res1

--       res1 :: f a
--       res1 = undefined
      -- res1 = runContraLift (contraLiftHandler coopHandler) cont3

      -- cont3
      --   :: forall w
      --    . (Functor w)
      --   => (forall x . f x -> eff (w x))
      --   -> eff (w a)
      -- cont3 contraLift3 = cont1 contraLift4
      --  where
      --   contraLift4
      --     :: forall x
      --      . ChurchMonad ops eff x
      --     -> eff (w x)
      --   contraLift4 = undefined
--         contraLift4 (ChurchMonad m3) = contraLift3 m4
--          where
--           m4 :: f x
--           m4 = m3 coopHandler $
--             returnHandler coopHandler
