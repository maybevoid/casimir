
module Control.Effect.Dynamic
  ( module Control.Effect.Dynamic.Class
  , module Control.Effect.Dynamic.Handler
  , module Control.Effect.Dynamic.Lift
  )
where

import Control.Effect.Dynamic.Class
import Control.Effect.Dynamic.Handler
import Control.Effect.Dynamic.Lift

-- mkDynamicHandler
--   :: forall ops1 ops2 a r eff1 .
--   ( EffOps ops1
--   , EffOps ops2
--   , Effect eff1
--   )
--   => (forall eff2 .
--       (Effect eff2)
--       => LiftEff eff1 eff2
--       -> (OpsConstraint ops1 eff2
--           => OpsHandler ops2 a r eff2))
--   -> DynamicHandler ops1 ops2 a r eff1
-- mkDynamicHandler handler = Computation $
--   \ liftEff ops -> bindConstraint ops $ handler liftEff

-- genericDynamicHandler
--   :: forall ops1 ops2 a r eff1.
--   ( EffOps ops1
--   , EffOps ops2
--   , Effect eff1
--   )
--   => (forall eff2 .
--       (Effect eff2, OpsConstraint ops1 eff2)
--       => OpsHandler ops2 a r eff2)
--   -> DynamicHandler ops1 ops2 a r eff1
-- genericDynamicHandler comp = mkDynamicHandler $ \_ -> comp

-- applyDynamic
--   :: forall ops1 ops2 a r eff .
--   ( EffOps ops1
--   , EffOps ops2
--   , DynamicOps ops1
--   , Effect eff
--   , OpsConstraint ops2 eff
--   )
--   => DynamicHandler ops2 ops1 a r eff
--   -> Computation ops1 (Return a) eff
--   -> eff r
-- applyDynamic handler1 comp1 = comp2
--  where
--   handler2 :: OpsHandler ops1 a r eff
--   handler2 = runComp handler1 id captureOps

--   comp2 :: eff r
--   comp2 = handleDynamic handler2 $ returnVal $
--     runComp comp1 liftDynamicEff dynamicOps
