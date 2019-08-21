implicit-effects Overview
=========================

> {-# LANGUAGE
>     GADTs
>   , DataKinds
>   , RankNTypes
>   , TypeFamilies
>   , InstanceSigs
>   , TypeOperators
>   , ExplicitForAll
>   , ImplicitParams
>   , KindSignatures
>   , ConstraintKinds
>   , TypeApplications
>   , ScopedTypeVariables
>   , QuantifiedConstraints
>   , MultiParamTypeClasses
> #-}
>

> import Prelude hiding (getLine)
> import Control.Monad.Trans.State (StateT (..))

> import qualified Prelude
> import qualified Control.Monad.Trans.Class as MonadTrans

> import Control.Effect.Implicit
> import Control.Effect.Implicit.Ops.Io
> import Control.Effect.Implicit.Ops.Env
> import Control.Effect.Implicit.Ops.State
> import Control.Effect.Implicit.Transform.State

Hello World
===========

- Use the effect operation `IoOps` for IO effects.
- Classic-style declaration with `Monad m`.
- `OpsConstraint IoOps m` lifts an `IoOps m` data into constraint.
- `liftIo` function provided to function under `OpsConstraint IoOps m`.

> hello11
>   :: forall m
>    . (Monad m, OpsConstraint IoOps m)
>   => m ()
> hello11 = liftIo $ putStrLn "hello world!"

- Type alias `Effect` as `Monad`.
- Rename type variable `m` to `eff`

> -- type Effect = Monad
> hello12
>   :: forall eff
>    . (Effect eff, OpsConstraint IoOps eff)
>   => eff ()
> hello12 = liftIo $ putStrLn "hello world!"

- Type alias `EffConstraint` as both `Effect` and `OpsConstraint`

> -- type EffConstraint ops eff = (Effect eff, OpsConstraint ops eff)
> hello13
>   :: forall eff
>    . (EffConstraint IoOps eff)
>   => eff ()
> hello13 = liftIo $ putStrLn "hello world!"

- Type alias `Eff` to abstract `forall eff`.

> -- type Eff ops a = forall eff . (EffConstraint ops eff) => eff a
> hello14 :: Eff IoOps ()
> hello14 = liftIo $ putStrLn "hello world!"

Multiple Effects
================

- Also use effect operation `EnvOps` for the reader / environment effect.
- `(∪)` or `Union` to combine effect operations.

> hello21
>   :: Eff (EnvOps String ∪ IoOps) ()
> hello21 = do
>   name <- ask
>   liftIo $ putStrLn $ "hello, " <> name <> "!"

Custom Effects
==============

- Create new effect `teletypeOps` by defining new operation data type
  parameterized by a monad `eff`.

> data TeletypeOps eff = TeletypeOps
>  { getLineOp :: eff String
>  , putLineOp :: String -> eff ()
>  }

- Implement `TeletypeOps` as instance of `ImplicitOps`
- Use implicit parameter to embed `TeletypeOps` into constraint.
- `captureOps` method captures the implicit parameter and return it as value.
- `withOps` takes a `TeletypeOps` and a continuation that depends on the
  implicit parameter and runs the continuation with the implicit parameter
  constraint discharged.

> instance ImplicitOps TeletypeOps where
>   type OpsConstraint TeletypeOps eff =
>     ?teletypeOps :: TeletypeOps eff
>
>   captureOps
>     :: forall eff
>      . (?teletypeOps :: TeletypeOps eff)
>     => TeletypeOps eff
>   captureOps = ?teletypeOps
>
>   withOps
>     :: forall eff r
>      . TeletypeOps eff
>     -> ((?teletypeOps :: TeletypeOps eff) => r)
>     -> r
>   withOps ops cont = let ?teletypeOps = ops in cont

- Implement helper functions to access effect operations from implicit parameters.

> getLine :: Eff TeletypeOps String
> getLine = getLineOp captureOps
>
> putLine :: String -> Eff TeletypeOps ()
> putLine = putLineOp captureOps

Computation
===========

- Terms with type `Eff` is universally quantified to work for all monad `eff`.
- Concrete semantics depends on the concrete monad type.
- Computations are abstract and need to be interpreted.

> hello31 :: Eff TeletypeOps ()
> hello31 = do
>   name <- getLine
>   putLine $ "hello, " <> name <> "!"

Effect Intepretation
====================

- Computations are interpreted by providing concrete effect operations.
- We know how to interpret the `TeletypeOps` operation under the `IO` monad.

> teletypeOps1 :: TeletypeOps IO
> teletypeOps1 = TeletypeOps
>   { getLineOp = Prelude.getLine
>   , putLineOp = Prelude.putStrLn
>   }

- Use `withOps` to bind the operation constraints and unify with the generic type `eff`.

> hello32 :: IO ()
> hello32 = withOps teletypeOps1 hello31

- We also know how to interpret `TeletypeOps` for any monad `eff` that supports the
  `IoOps` operation.

> teletypeOps2
>   :: forall eff
>    . (EffConstraint IoOps eff)
>   => TeletypeOps eff
> teletypeOps2 = TeletypeOps
>   { getLineOp = liftIo $ Prelude.getLine
>   , putLineOp = liftIo . Prelude.putStrLn
>   }

- Use the built in `ioOps` to interpret `IoOps` under `IO`.
- The inner `withOps` captures the `IoOps` constraint and implements a
  concrete `TeletypeOps` that work with `IO`.

> hello33 :: IO ()
> hello33 = withOps ioOps $
>   withOps teletypeOps2 $
>     hello31

Effect Lifting
==============

> instance EffFunctor TeletypeOps where
>   effmap
>     :: forall eff1 eff2
>      . (Effect eff1, Effect eff2)
>     => (forall x . eff1 x -> eff2 x)
>     -> TeletypeOps eff1
>     -> TeletypeOps eff2
>   effmap lifter teletypeOps = TeletypeOps
>     { getLineOp = lifter $ getLineOp teletypeOps
>     , putLineOp = lifter . (putLineOp teletypeOps)
>     }

> teletypeOps3 :: TeletypeOps (StateT String IO)
> teletypeOps3 = effmap MonadTrans.lift teletypeOps1

Multiple Effects
================

> hello7 :: Eff (TeletypeOps ∪ StateOps String) ()
> hello7 = do
>   name <- get
>   putLine $ "hello, " <> name <> "!"

> hello8 :: StateT String IO ()
> hello8 = withOps (stateTOps ∪ teletypeOps3) hello7