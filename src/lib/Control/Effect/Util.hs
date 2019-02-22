
module Control.Effect.Util where

import Control.Effect.Class
import Control.Effect.Union

composeEffHandlers
  :: forall eff effRow1 effRow2 .
    ( Effect eff
    , EffRow effRow1
    , EffRow effRow2
    )
  => effRow1 eff
  -> (EffConstraint effRow1 eff => effRow2 eff)
  -> Union effRow1 effRow2 eff
composeEffHandlers effRow1 effRow2 =
  bindConstraint effRow1 $
    Union effRow1 effRow2

stackEffHandlers
  :: forall
      eff1      -- outer effect
      eff2      -- inner effect
      effRow1
      effRow2 .
    ( Effect eff1
    , Effect eff2
    , EffRow effRow1
    , EffRow effRow2
    )
  => effRow1 eff1
  -> (forall eff . effRow2 eff)
  -> LiftEff eff2 eff1
  -> Union effRow1 effRow2 eff1
stackEffHandlers effRow1 effRow2 lift21 =
  composeEffHandlers effRow1 effRow2'
  where
    effRow2' :: effRow2 eff1
    effRow2' = effmap lift21 effRow2
