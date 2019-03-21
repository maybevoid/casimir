module Control.Effect.Tag.Env
where

import Control.Effect.Base
import Control.Effect.Ops.Env

type TaggedEnvEff tag e = TaggedEff tag (EnvEff e)
type TaggedEnvOps tag e eff = TaggedOps tag (EnvOps e) eff
type TaggedEnvCoOp tag e r = TaggedCoOp tag (EnvCoOp e) r

askTag
  :: forall tag e eff
   . ( Effect eff
     , EffOps (TaggedEnvEff tag e)
     , OpsConstraint (TaggedEnvEff tag e) eff
     )
  => eff e
askTag = withTag @tag @(EnvEff e) $ ask

mkTaggedEnvOps
  :: forall tag e eff
   . (Effect eff)
   => e
   -> TaggedEnvOps tag e eff
mkTaggedEnvOps = TaggedOps . mkEnvOps