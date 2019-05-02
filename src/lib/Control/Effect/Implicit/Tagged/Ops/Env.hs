module Control.Effect.Implicit.Tagged.Ops.Env
where

import Control.Effect.Implicit.Base
import Control.Effect.Implicit.Ops.Env
import Control.Effect.Implicit.Tagged.TaggedEff

type TaggedEnvEff tag e = TaggedEff tag (EnvEff e)
type TaggedEnvOps tag e eff = TaggedOps tag (EnvOps e) eff
type TaggedEnvCoOp tag e r = TaggedCoOp tag (EnvCoOp e) r

askTag
  :: forall tag e eff
   . ( ImplicitOps (TaggedEnvEff tag e)
     , EffConstraint (TaggedEnvEff tag e) eff
     )
  => eff e
askTag = withTag @tag @(EnvEff e) ask

mkTaggedEnvOps
  :: forall tag eff e
   . (Effect eff)
   => e
   -> TaggedEnvOps tag e eff
mkTaggedEnvOps = TaggedOps . mkEnvOps

withTaggedEnv
  :: forall tag r e eff
   . (Effect eff, ImplicitOps (TaggedEnvEff tag e))
  => e
  -> ((OpsConstraint (TaggedEnvEff tag e) eff) => eff r)
  -> eff r
withTaggedEnv e cont =
  withOps (mkTaggedEnvOps @tag @eff e) cont
