module Control.Effect.Implicit.Computation
  ( Computation (..)
  , OpsHandler
  , FunctorComp (..)
  , Return (..)
  , Arrow (..)
  , ReturnCtx (..)
  , Pipeline (..)
  , TransformerHandler (..)
  , GenericReturn
  , IdentityComputation
  , BaseOpsHandler
  , GenericOpsHandler
  , SimplePipeline
  , GenericPipeline
  , genericComputation
  , genericReturn
  , arrowComputation
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
  , composeExactPipelines
  , castComputation
  , castOpsHandler
  , bindOpsHandler
  , bindOpsHandlerWithCast
  , composeOpsHandlers
  , composeOpsHandlersWithCast
  , runPipeline
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