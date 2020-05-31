
module Casimir.Ops.State.Default
  ( StateTag
  , StateEff
  , StateOps (..)
  , get
  , put
  , Label.getOp
  , Label.putOp
  , pattern Label.StateOps
  )
where

import Data.Kind

import Data.QuasiParam.Tag
import Casimir.Base

import qualified Casimir.Ops.State.Base as Base
import qualified Casimir.Ops.State.Label as Label

data StateTag

type StateEff s = Label.StateEff Type StateTag s
type StateOps s = Label.StateOps Type StateTag s

{-# INLINE get #-}
get :: forall s . Eff (StateEff s) s
get = Label.get @Type @StateTag

{-# INLINE put #-}
put :: forall s . s -> Eff (StateEff s) ()
put = Label.put @Type @StateTag
