module Control.Effect.Implicit.Computation
  ( Computation (..)
  , Handler
  , Return (..)
  , Pipeline (..)
  , TransformerHandler (..)
  , Cast (..)
  , OpsCast
  , type (⊇)
  , GenericReturn
  , GenericComputation
  , IdentityComputation
  , BaseHandler
  , GenericHandler
  , SimplePipeline
  , GenericPipeline
  , returnComputation
  , genericComputation
  , genericReturn
  , runIdentityComp
  , execComp
  , liftComputation
  , mkHandler
  , withHandler
  , baseHandler
  , genericHandler
  , bindExactHandler
  , composeExactHandlers
  , handlerToPipeline
  , transformerPipeline
  , composePipelines
  , cast
  , runCast
  , castOps
  , castComputation
  , castHandler
  , bindHandlerWithCast
  , composeHandlersWithCast
  , extendCast
  , composeCast
  , runPipelineWithCast
  , castPipelineOps
  , castPipelineHandler
  , composePipelinesWithCast
  )
where

import Control.Effect.Implicit.Computation.Class
import Control.Effect.Implicit.Computation.Lift
import Control.Effect.Implicit.Computation.Value
import Control.Effect.Implicit.Computation.Handler
import Control.Effect.Implicit.Computation.Cast
import Control.Effect.Implicit.Computation.Pipeline