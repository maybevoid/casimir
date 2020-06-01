
module Casimir.Base.ContraLift
  ( ContraLift (..)
  , joinContraLift
  , identityContraLift
  )
where

import Control.Monad.Identity
import Data.Functor.Compose

newtype ContraLift m1 m2 = ContraLift {
  runContraLift
    :: forall a
     . (forall w
         . (Functor w)
        => (forall x . m2 x -> m1 (w x))
        -> m1 (w a))
    -> m2 a
}

identityContraLift
  :: forall m . Monad m
  => ContraLift m m
identityContraLift = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . ((forall x . m x -> m (Identity x))
        -> m (Identity a))
    -> m a
  contraLift1 cont =
    fmap runIdentity $ cont $ fmap Identity

joinContraLift
  :: forall m1 m2 m3
   . ( Monad m1
     , Monad m2
     , Monad m3
     )
  => ContraLift m1 m2
  -> ContraLift m2 m3
  -> ContraLift m1 m3
joinContraLift contraLift1 contraLift2 = ContraLift contraLift3
 where
  contraLift3
    :: forall a
     . (forall w
         . (Functor w)
        => (forall x . m3 x -> m1 (w x))
        -> m1 (w a))
    -> m3 a
  contraLift3 cont1 = runContraLift contraLift2 cont2
   where
    cont2
      :: forall w1
       . (Functor w1)
      => (forall x . m3 x -> m2 (w1 x))
      -> m2 (w1 a)
    cont2 contraLift4 = runContraLift contraLift1 cont3
     where
      cont3
        :: forall w2
         . (Functor w2)
        => (forall x . m2 x -> m1 (w2 x))
        -> m1 (w2 (w1 a))
      cont3 contraLift5 = do
        (Compose mx) <- cont1 contraLift6
        return mx
       where
        contraLift6
          :: forall x . m3 x -> m1 (Compose w2 w1 x)
        contraLift6 mx1 = do
          mx2 <- contraLift5 $ contraLift4 mx1
          return $ Compose mx2
