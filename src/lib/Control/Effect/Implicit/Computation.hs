module Control.Effect.Implicit.Computation
  ( Computation (..)
  , OpsHandler
  , Return (..)
  , Pipeline (..)
  , TransformerHandler (..)
  , Cast (..)
  , OpsCast
  , type (⊇)
  , GenericReturn
  , IdentityComputation
  , BaseOpsHandler
  , GenericOpsHandler
  , SimplePipeline
  , GenericPipeline
  , genericComputation
  , genericReturn
  , runIdentityComp
  , execComp
  , liftComputation
  , bindOps
  , opsHandlerComp
  , withOpsHandler
  , baseOpsHandler
  , genericOpsHandler
  , bindExactOpsHandler
  , composeExactOpsHandlers
  , opsHandlerToPipeline
  , transformePipeline
  , composePipelines
  , cast
  , runCast
  , castOps
  , castComputation
  , castOpsHandler
  , bindOpsHandlerWithCast
  , composeOpsHandlersWithCast
  , extendCast
  , composeCast
  , runPipelineWithCast
  , castPipelineOps
  , castPipelineHandler
  , composePipelinesWithCast
  )
where

import Control.Effect.Implicit.Computation.Computation
import Control.Effect.Implicit.Computation.Value
import Control.Effect.Implicit.Computation.Handler
import Control.Effect.Implicit.Computation.Cast
import Control.Effect.Implicit.Computation.Pipeline