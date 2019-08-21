{-# LANGUAGE PolyKinds #-}

module Control.Effect.Implicit.Base.Label
  ( OpsParam
  , TaggedOpsParam
  , NamedOpsParam
  , module Control.Implicit.Param
  )
where

import GHC.Types

import Control.Implicit.Param

type OpsParam k (label :: k) ops eff =
  ImplicitParam k label (ops eff)

type TaggedOpsParam label ops eff = OpsParam Type label ops eff
type NamedOpsParam label ops eff = OpsParam Symbol label ops eff
