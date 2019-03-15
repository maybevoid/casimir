
module Control.Effect.Base.Effect
  ( type (~>)
  , Effect
  )
where

import Control.Natural (type (~>))

type Effect eff = Monad eff

