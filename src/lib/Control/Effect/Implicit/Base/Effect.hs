
module Control.Effect.Implicit.Base.Effect
  ( type (~>)
  , Effect
  )
where

import Control.Natural (type (~>))

type Effect eff = Monad eff

