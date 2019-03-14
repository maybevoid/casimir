
module Control.Effect.Dynamic.Lift
where

import Control.Effect.Base
import Control.Effect.Dynamic.Class

liftOps
  :: forall ops eff a .
  ( Effect eff
  , EffOps ops
  )
  => CoOperation ops (eff a)
  -> DynamicEff ops eff a
liftOps ops = DynamicEff $ cont
 where
  cont :: forall r . OpsHandler ops a r eff -> eff r
  cont handler = handleOps handler $ fmap mapper ops
   where
    mapper :: eff a -> eff r
    mapper mx = do
      x <- mx
      handleReturn handler x
