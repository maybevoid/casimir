{-# language PolyKinds #-}

module Casimir.Base.Cast
  ( EntailOps
  , CastOps
  , type (⊇)
  , CastDict
  , EntailDict
  , entailDict
  , extendCast
  , composeCast
  , castOpsWithDict
  , castOps
  , castDict
  )
where

import Casimir.Base.Effect
import qualified Casimir.Param as Param

class
  ( Effects ops1
  , Effects ops2
  , Param.EntailParam ops1 ops2 m
  )
  => EntailOps ops1 ops2 m

instance
  ( Effects ops1
  , Effects ops2
  , Param.EntailParam ops1 ops2 m
  )
  => EntailOps ops1 ops2 m

class
  ( Effects ops1
  , Effects ops2
  , Param.CastParam ops1 ops2
  )
  => CastOps ops1 ops2

instance
  ( Effects ops1
  , Effects ops2
  , Param.CastParam ops1 ops2
  )
  => CastOps ops1 ops2

infixl 6 ⊇
type ops1 ⊇ ops2 = CastOps ops1 ops2

type EntailDict ops1 ops2 m
  = Param.EntailDict ops1 ops2 m

type CastDict ops1 ops2
  = Param.CastDict ops1 ops2

castOps
  :: forall ops1 ops2 m
   . ( CastOps ops1 ops2 )
  => ops1 m
  -> ops2 m
castOps = Param.castValue

castOpsWithDict
  :: forall ops1 ops2 m
   . ( Effects ops1
     , Effects ops2
     )
  => CastDict ops1 ops2
  -> ops1 m
  -> ops2 m
castOpsWithDict = Param.castValueWithDict

extendCast
  :: forall ops1 ops2 ops3
   . CastDict ops1 ops2
  -> CastDict (Union ops1 ops3) (Union ops2 ops3)
extendCast = Param.extendCast @ops1

entailDict
  :: forall ops1 ops2 m
   . (EntailOps ops1 ops2 m)
  => EntailDict ops1 ops2 m
entailDict = Param.entailDict @ops1

castDict
  :: forall ops1 ops2
   . (CastOps ops1 ops2)
  => CastDict ops1 ops2
castDict = Param.castDict @ops1

composeCast
  :: forall ops1 ops2 ops3
   . CastDict ops1 ops2
  -> CastDict ops2 ops3
  -> CastDict ops1 ops3
composeCast = Param.composeCast @ops1 @ops2
