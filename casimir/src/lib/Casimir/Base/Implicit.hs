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

import qualified Data.QuasiParam.Multi as Multi

class
  ( Multi.MultiParam (Type -> Type) (AsMultiParam ops)
  )
  => ParamOps ops where
    type family AsMultiParam ops
      = (param :: (Type -> Type) -> Type) | param -> ops

    coerceTo
      :: forall m
       . ops m -> AsMultiParam ops m

    coerceFrom
      :: forall m
       . AsMultiParam ops m
      -> ops m

type OpsParam ops m =
  Multi.ParamConstraint
    (Type -> Type)
    (AsMultiParam ops)
    m

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
  :: forall ops m r
   . ( ParamOps ops )
  => ops m
  -> (OpsParam ops m => r)
  -> r
withOps' ops cont = Multi.withParam ops' cont
 where
  ops' :: AsMultiParam ops m
  ops' = coerceTo ops

captureOps'
  :: forall ops m
   . ( ParamOps ops
     , OpsParam ops m
     )
  => ops m
captureOps' = coerceFrom ops'
 where
  ops' :: AsMultiParam ops m
  ops' = Multi.captureParam @(Type -> Type) @(AsMultiParam ops)

withOps
  :: forall ops m r
   . ( ImplicitOps ops )
  => Operation ops m
  -> (OpsConstraint ops m => r)
  -> r
withOps = withOps'

captureOps
  :: forall ops m
   . ( OpsConstraint ops m )
  => Operation ops m
captureOps = captureOps'

type ImplicitOps ops =
  ( EffOps ops
  , ParamOps (Operation ops)
  )

type OpsConstraint ops m =
  ( ImplicitOps ops
  , OpsParam (Operation ops) m
  )

type EffConstraint ops m = (Monad m, OpsConstraint ops m)

type Eff ops a = forall m . (EffConstraint ops m) => m a


-- | 'ParamOps' gives computations access to mect operations of an
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
-- The definition for 'ParamOps' for most mect operations can typically
-- be derived mechanically. We may look into using template Haskell to generate
-- instances for 'ParamOps' in future to reduce some boilerplate.
-- class
--   (EffOps ops)
--   => ParamOps ops where

--     -- | The constraint kind for the mect operation under 'Monad' @m@.
--     -- This is typically an implicit parameter with a unique name, e.g.
--     -- @type OpsParam FooOps m = (?fooOps :: FooOps m)@.
--     --
--     -- Note that there is a injective type families condition, and given that
--     -- implicit parameters have a single namespace, users must come out with
--     -- naming conventions for their custom mects to avoid name clashing
--     -- that would result in compile-time injectivity violation error.
--     type family OpsParam ops (m :: Type -> Type)
--       = (c :: Constraint) | c -> ops m

--     -- | Takes an mect operation @'Operation' ops m@ and bind it to the
--     -- implicit parameter specified in @'OpsParam' ops m@ for the
--     -- continuation @r@. The expression @r@ can then use the mect operations
--     -- without having to explicitly pass them around as function arguments.
--     -- For the example @FooEff@, the body for 'withOps' can be defined as
--     -- @withOps fooOps cont = let ?fooOps = fooOps in cont @.
--     withOps
--       :: forall m r
--        . (Monad m)
--       => Operation ops m
--       -> (OpsParam ops m => r)
--       -> r

--     -- | If an implicit parameter for the mect operation is available in the
--     -- context, capture it and return the operation as a value. For the example
--     -- @FooEff@, the body for 'captureOps' can be defined as @captureOps = ?fooOps@.
--     captureOps
--       :: forall m
--        . (Monad m, OpsParam ops m)
--       => Operation ops m

-- -- | This is a type alias for the implicit parameter constraint for @ops@,
-- -- in addition to requiring @m@ to be an 'Monad'. This helps reducing
-- -- boilerplate in generic computations so that we do not have to keep
-- -- repeating the @(Monad m)@ constraint in our type signatures.
