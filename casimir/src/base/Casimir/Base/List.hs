{-# language PolyKinds #-}

module Casimir.Base.List
  ( Effects (..)
  , pattern Cons
  )
where

import Casimir.Base.Effect
import qualified QuasiParam.Casimir as Param

type ConsOps ops1 ops2 = Param.Cons ops1 ops2

pattern Cons
  :: forall ops1 ops2 m
   . ops1 m
  -> ops2 m
  -> ConsOps ops1 ops2 m
pattern Cons ops1 ops2 = Param.Cons ops1 ops2
