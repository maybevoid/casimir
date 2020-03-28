
module Control.Effect.Implicit.Higher.ContraLift
where

import Data.Functor.Compose
import Control.Monad.Identity

import Control.Effect.Implicit.Base (Effect, ContraLift (..))

type ContraFree eff f =
  forall a
   . (forall w
       . (Functor w)
     => (forall x . f (eff x) -> eff (w x))
     -> eff (w (eff (f a))))
  -> eff (f a)

identityContraLift
  :: forall eff . Effect eff
  => ContraLift eff eff
identityContraLift = ContraLift contraLift1
 where
  contraLift1
    :: forall a
     . ((forall x . eff x -> eff (Identity x))
        -> eff (Identity a))
    -> eff a
  contraLift1 cont =
    fmap runIdentity $ cont $ fmap Identity

joinContraLift
  :: forall eff1 eff2 eff3
   . ( Effect eff1
     , Effect eff2
     , Effect eff3
     )
  => ContraLift eff1 eff2
  -> ContraLift eff2 eff3
  -> ContraLift eff1 eff3
joinContraLift contraLift1 contraLift2 = ContraLift contraLift3
 where
  contraLift3
    :: forall a
     . (forall w
         . (Functor w)
        => (forall x . eff3 x -> eff1 (w x))
        -> eff1 (w a))
    -> eff3 a
  contraLift3 cont1 = runContraLift contraLift2 cont2
   where
    cont2
      :: forall w1
       . (Functor w1)
      => (forall x . eff3 x -> eff2 (w1 x))
      -> eff2 (w1 a)
    cont2 contraLift4 = runContraLift contraLift1 cont3
     where
      cont3
        :: forall w2
         . (Functor w2)
        => (forall x . eff2 x -> eff1 (w2 x))
        -> eff1 (w2 (w1 a))
      cont3 contraLift5 = do
        (Compose mx) <- cont1 contraLift6
        return mx
       where
        contraLift6
          :: forall x . eff3 x -> eff1 (Compose w2 w1 x)
        contraLift6 mx1 = do
          mx2 <- contraLift5 $ contraLift4 mx1
          return $ Compose mx2
