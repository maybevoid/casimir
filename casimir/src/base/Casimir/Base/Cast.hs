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
import Casimir.Base.Implicit
import qualified QuasiParam.Casimir as Param

class
  ( ImplicitOps eff1
  , ImplicitOps eff2
  , Param.EntailParam (Operations eff1) (Operations eff2) m
  )
  => EntailOps eff1 eff2 m

instance
  ( ImplicitOps eff1
  , ImplicitOps eff2
  , Param.EntailParam (Operations eff1) (Operations eff2) m
  )
  => EntailOps eff1 eff2 m

class
  ( ImplicitOps eff1
  , ImplicitOps eff2
  , Param.CastParam (Operations eff1) (Operations eff2)
  )
  => CastOps eff1 eff2

instance
  ( ImplicitOps eff1
  , ImplicitOps eff2
  , Param.CastParam (Operations eff1) (Operations eff2)
  )
  => CastOps eff1 eff2

infixl 6 ⊇
type eff1 ⊇ eff2 = CastOps eff1 eff2

type EntailDict eff1 eff2 m
  = Param.EntailDict (Operations eff1) (Operations eff2) m

type CastDict eff1 eff2
  = Param.CastDict (Operations eff1) (Operations eff2)

castOps
  :: forall eff1 eff2 m
   . ( CastOps eff1 eff2 )
  => Operations eff1 m
  -> Operations eff2 m
castOps = Param.castValue

castOpsWithDict
  :: forall eff1 eff2 m
   . ( ImplicitOps eff1
     , ImplicitOps eff2
     )
  => CastDict eff1 eff2
  -> Operations eff1 m
  -> Operations eff2 m
castOpsWithDict = Param.castValueWithDict

extendCast
  :: forall eff1 eff2 eff3
   . CastDict eff1 eff2
  -> CastDict (Union eff1 eff3) (Union eff2 eff3)
extendCast = Param.extendCast @(Operations eff1)

entailDict
  :: forall eff1 eff2 m
   . (EntailOps eff1 eff2 m)
  => EntailDict eff1 eff2 m
entailDict = Param.entailDict @(Operations eff1)

castDict
  :: forall eff1 eff2
   . (CastOps eff1 eff2)
  => CastDict eff1 eff2
castDict = Param.castDict @(Operations eff1)

composeCast
  :: forall eff1 eff2 eff3
   . CastDict eff1 eff2
  -> CastDict eff2 eff3
  -> CastDict eff1 eff3
composeCast = Param.composeCast @(Operations eff1) @(Operations eff2)
