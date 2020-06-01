{-# LANGUAGE PolyKinds #-}

module Casimir.Ops.State.Label
  ( StateEff
  , StateOps (..)
  , get
  , put
  , getOp
  , putOp
  , pattern StateOps
  )
where

import Data.QuasiParam.Tag
import Casimir.Base

import qualified Casimir.Ops.State.Base as Base

type StateEff k label s = LabeledEff k label (Base.StateEff s)
type StateOps k label s = LabeledOps k label (Base.StateOps s)

pattern StateOps
  :: forall k (label :: k) s m
   . m s
  -> (s -> m ())
  -> StateOps k label s m
pattern StateOps { getOp, putOp }
  = LabeledOps (Base.StateOps getOp putOp)

{-# INLINE get #-}
get :: forall k (label :: k) s . Eff (StateEff k label s) s
get = getOp $ captureOps @(StateEff k label s)

{-# INLINE put #-}
put :: forall k (label :: k) s . s -> Eff (StateEff k label s) ()
put = putOp $ captureOps @(StateEff k label s)
