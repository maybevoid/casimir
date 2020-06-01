
module Casimir.Higher.ContraLift.State
where

import Casimir.Base
import Casimir.Higher.ContraLift

data CoState s m a = CoState {
  runCoState :: s -> m (s, a)
} deriving (Functor)

contraState
  :: forall s m
   . (Monad m)
  => ContraFree m (CoState s m)
contraState = handler1
 where
  handler1
    :: forall a
     . ((forall x . CoState s m (m x) -> m (s, x))
        -> m (s, m (CoState s m a)))
    -> m (CoState s m a)
  handler1 cont1 = return $ CoState handler2
   where
    handler2 :: s -> m (s, a)
    handler2 s1 = do
      (s2, cont2) <- cont1 handler3
      cont3 <- cont2
      runCoState cont3 s2
     where
      handler3 :: CoState s m (m x) -> m (s, x)
      handler3 (CoState cont3) = do
        (s2, mx) <- cont3 s1
        x <- mx
        return (s2, x)
