module Casimir.Base.Cast
  ( EntailOps
  , CastOps
  , type (⊇)
  , castOps
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
type ops1 ⊇ ops2 = CastOps ops1 ops2

castOps
  :: forall eff1 eff2 m
   . ( CastOps eff1 eff2 )
  => Operations eff1 m
  -> Operations eff2 m
castOps = Param.castValue
