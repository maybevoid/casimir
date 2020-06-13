
module Casimir.Freer.FreeTransformer
  ( FreeTransformer (..)
  , CoOpHandler (..)
  )
where

import Casimir.Freer.FreeOps

data CoOpHandler ops a r m =
  CoOpHandler
    { returnHandler :: a -> m r
    , coOpHandler
        :: forall x
        . CoOperation ops x
        -> (x -> (m r))
        -> m r
    }

class
  (forall ops m . (FreeOps ops, Monad m) => Monad (free ops m))
  => FreeTransformer free where
    freeOps :: forall ops m
       . (FreeOps ops, Monad m)
      => ops (free ops m)

    liftFree :: forall ops m a
       . (FreeOps ops, Monad m)
      => m a
      -> free ops m a

    handleFree
      :: forall ops m a r
       . (Monad m, FreeOps ops)
      => CoOpHandler ops a r m
      -> free ops m a
      -> m r
