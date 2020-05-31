{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Casimir.Base.Implicit
  ( ImplicitOps
  , OpsConstraint
  , EffConstraint
  , Eff
  , withOps
  , captureOps
  )
where

import Data.Kind
import Data.Coerce

import Casimir.Base.NoOp
import Casimir.Base.Union
import Casimir.Base.Label
import Casimir.Base.EffOps
import Casimir.Base.Effect

import qualified Data.QuasiParam.Multi as Multi

class
  ( Multi.MultiParam (Type -> Type) (AsMultiParam ops)
  )
  => ParamOps ops where
    type family AsMultiParam ops
      = (param :: (Type -> Type) -> Type) | param -> ops

    coerceTo
      :: forall eff
       . ops eff -> AsMultiParam ops eff

    coerceFrom
      :: forall eff
       . AsMultiParam ops eff
      -> ops eff

type OpsParam ops eff =
  Multi.ParamConstraint
    (Type -> Type)
    (AsMultiParam ops)
    eff

instance ParamOps NoOp where
  type AsMultiParam NoOp = Multi.Empty (Type -> Type)

  coerceTo = coerce
  coerceFrom = coerce

instance ParamOps (LabeledOps k label ops) where
  type AsMultiParam (LabeledOps k label ops) =
    Multi.Elem k (Type -> Type) label ops

instance
  ( ParamOps ops1
  , ParamOps ops2
  )
  => ParamOps (UnionOps ops1 ops2) where
    type AsMultiParam (UnionOps ops1 ops2) =
      Multi.Cons
        (Type -> Type)
        (AsMultiParam ops1)
        (AsMultiParam ops2)

withOps'
  :: forall ops eff r
   . ( ParamOps ops )
  => ops eff
  -> (OpsParam ops eff => r)
  -> r
withOps' ops cont = Multi.withParam ops' cont
 where
  ops' :: AsMultiParam ops eff
  ops' = coerceTo ops

captureOps'
  :: forall ops eff
   . ( ParamOps ops
     , OpsParam ops eff
     )
  => ops eff
captureOps' = coerceFrom ops'
 where
  ops' :: AsMultiParam ops eff
  ops' = Multi.captureParam @(Type -> Type) @(AsMultiParam ops)

withOps
  :: forall ops eff r
   . ( ImplicitOps ops )
  => Operation ops eff
  -> (OpsConstraint ops eff => r)
  -> r
withOps = withOps'

captureOps
  :: forall ops eff
   . ( OpsConstraint ops eff )
  => Operation ops eff
captureOps = captureOps'

type ImplicitOps ops =
  ( EffOps ops
  , ParamOps (Operation ops)
  )

type OpsConstraint ops eff =
  ( ImplicitOps ops
  , OpsParam (Operation ops) eff
  )

type EffConstraint ops eff = (Effect eff, OpsConstraint ops eff)

type Eff ops a = forall eff . (EffConstraint ops eff) => eff a


-- | 'ParamOps' gives computations access to effect operations of an
-- operation through implicit parameter constraints. It hides the machinery
-- of implicit parameters and make them appear like regular constraints except
-- with local scope.
--
-- The law for 'ParamOps' is
--
-- @
-- forall ops . 'withOps' ops 'captureOps' === ops
-- @
--
-- This means any non-trivial instance for 'ParamOps' must somehow make use of
-- implicit parameters for the law to hold.
--
-- The definition for 'ParamOps' for most effect operations can typically
-- be derived mechanically. We may look into using template Haskell to generate
-- instances for 'ParamOps' in future to reduce some boilerplate.
-- class
--   (EffOps ops)
--   => ParamOps ops where

--     -- | The constraint kind for the effect operation under 'Effect' @eff@.
--     -- This is typically an implicit parameter with a unique name, e.g.
--     -- @type OpsParam FooOps eff = (?fooOps :: FooOps eff)@.
--     --
--     -- Note that there is a injective type families condition, and given that
--     -- implicit parameters have a single namespace, users must come out with
--     -- naming conventions for their custom effects to avoid name clashing
--     -- that would result in compile-time injectivity violation error.
--     type family OpsParam ops (eff :: Type -> Type)
--       = (c :: Constraint) | c -> ops eff

--     -- | Takes an effect operation @'Operation' ops eff@ and bind it to the
--     -- implicit parameter specified in @'OpsParam' ops eff@ for the
--     -- continuation @r@. The expression @r@ can then use the effect operations
--     -- without having to explicitly pass them around as function arguments.
--     -- For the example @FooEff@, the body for 'withOps' can be defined as
--     -- @withOps fooOps cont = let ?fooOps = fooOps in cont @.
--     withOps
--       :: forall eff r
--        . (Effect eff)
--       => Operation ops eff
--       -> (OpsParam ops eff => r)
--       -> r

--     -- | If an implicit parameter for the effect operation is available in the
--     -- context, capture it and return the operation as a value. For the example
--     -- @FooEff@, the body for 'captureOps' can be defined as @captureOps = ?fooOps@.
--     captureOps
--       :: forall eff
--        . (Effect eff, OpsParam ops eff)
--       => Operation ops eff

-- -- | This is a type alias for the implicit parameter constraint for @ops@,
-- -- in addition to requiring @eff@ to be an 'Effect'. This helps reducing
-- -- boilerplate in generic computations so that we do not have to keep
-- -- repeating the @(Effect eff)@ constraint in our type signatures.
