
module Casimir.Higher.ContraLift.State
where

import Casimir.Higher.ContraLift

data CoState s eff a = CoState {
  runCoState :: s -> eff (s, a)
} deriving (Functor)

contraState
  :: forall s eff
   . (Monad eff)
  => ContraFree eff (CoState s eff)
contraState = handler1
 where
  handler1
    :: forall a
     . ((forall x . CoState s eff (eff x) -> eff (s, x))
        -> eff (s, eff (CoState s eff a)))
    -> eff (CoState s eff a)
  handler1 cont1 = return $ CoState handler2
   where
    handler2 :: s -> eff (s, a)
    handler2 s1 = do
      (s2, cont2) <- cont1 handler3
      cont3 <- cont2
      runCoState cont3 s2
     where
      handler3 :: CoState s eff (eff x) -> eff (s, x)
      handler3 (CoState cont3) = do
        (s2, mx) <- cont3 s1
        x <- mx
        return (s2, x)
