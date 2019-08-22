implicit-effects Overview
=========================

- First we need to enable some language extensions.

> {-# LANGUAGE
>     GADTs
>   , DataKinds
>   , RankNTypes
>   , TypeFamilies
>   , InstanceSigs
>   , DeriveFunctor
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
> import Control.Monad.Identity
> import Data.Traversable
> import Control.Monad.Trans.State (StateT (..), evalStateT)

> import qualified Prelude
> import qualified Control.Monad.Trans.Class as MonadTrans

- The main module for implicit-effects.

> import Control.Effect.Implicit
> import Control.Effect.Implicit.Free
> import Control.Effect.Implicit.Free.ChurchMonad

- Effect operations are imported from separate modules

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

- Given an operation `TeletypeOps IO`, we want to lift it to work on other monads
  lifted by monad transformers, e.g. `TeletypeOps (StateT String IO)`.
- More generally, given any `TeletypeOps eff1`, we should be able to get a
  `TeletypeOps eff2` if there is a _natural transformation_ from `eff1` to `eff2`.
- The `EffFunctor` class defines the method `effmap` to do this for any effect
  operations.

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

- Now we can get our `TeletypeOps (StateT String IO)` by applying `effmap` with
  the `MonadTrans` `lift` method from `StateT`.

> teletypeOps3 :: TeletypeOps (StateT String IO)
> teletypeOps3 = effmap MonadTrans.lift teletypeOps1

- We can now use our new effect `TeletypeOps` with existing effects such as
  `StateOps`.

> hello41 :: Eff (TeletypeOps ∪ StateOps String) ()
> hello41 = do
>   name <- get
>   putLine $ "hello, " <> name <> "!"

- To interpret our effect, we instantiate the concrete monad to `StateT String IO`.
- `stateTOps` is the built in operation to interpret `StateOps` under the `StateT`
  monad transformer.
- Operations such as `stateTOps` provide a bridge to existing effect libraries
  such as `mtl`, allowing applications to mix different strategies for effect
  interpretation.

> hello42 :: StateT String IO ()
> hello42 = withOps (stateTOps ∪ teletypeOps3) hello41

Algebraic Effects
=================

- Algebraic effects is best understood from the [Eff language](https://www.eff-lang.org/).
- We can use algebraic effects in Haskell as well.
- Define a `TeletypeCoOp` data type, which is the _co-operation_ (dual) of `TeletypeOps`.

> data TeletypeCoOp r
>   = GetLineOp (String -> r)
>   | PutLineOp String (() -> r)
>   deriving Functor

- Declare `TeletypeOps` to be instance of `EffCoOp`, specifying `TeletypeCoOp`
  as its `CoOperation` type.

> instance EffCoOp TeletypeOps where
>   type CoOperation TeletypeOps = TeletypeCoOp

- Being an instance of `FreeOps`, we can construct a `TeletypeOps` for any
  given free monad `eff`, provided we know how to inject a `TeletypeCoOp`
  action into `eff`.

> instance FreeOps TeletypeOps where
>   mkFreeOps
>     :: forall eff
>      . (Effect eff)
>     => (forall r . TeletypeCoOp r -> eff r)
>     -> TeletypeOps eff
>   mkFreeOps liftCoOp = TeletypeOps
>     { getLineOp = liftCoOp $ GetLineOp id
>     , putLineOp = \x -> liftCoOp $ PutLineOp x id
>     }

- We can implement a naive co-op handler for `TeletypeOps` to handle the
  teletype effect in IO.

> teletypeHandler1
>   :: forall a . CoOpHandler TeletypeOps a a IO
> teletypeHandler1 = CoOpHandler handleReturn handleOp
>  where
>    handleReturn :: a -> IO a
>    handleReturn = return
>
>    handleOp :: TeletypeCoOp (IO a) -> IO a
>    handleOp (GetLineOp cont) = Prelude.getLine >>= cont
>    handleOp (PutLineOp x cont) = Prelude.putStrLn x >>= cont

- We can interpret a computation by applying the co-op handler using
  `withCoOpHandler`.

> hello51 :: Eff TeletypeOps ()
> hello51 = do
>   putLine "Enter first name:"
>   firstName <- getLine
>   putLine "Enter last name:"
>   lastName <- getLine
>   putLine $ "Hello, " <> firstName <> " " <> lastName <> "!"

> hello52 :: IO ()
> hello52 = withCoOpHandler teletypeHandler1 hello51

- There is not much benefit doing algebraic style if all we need is
  to work with the IO version of getLine and putLine.
- The real power of algebraic effects is that we can change the return
  type of the computation as well as calling the continuation zero or
  more times.
- We will build a new co-op handler that read the inputs from a state
  containing list of strings, and return a string representing the
  console interation.

> teletypeHandler2
>   :: forall a eff
>    . (EffConstraint (StateOps [String]) eff)
>   => CoOpHandler TeletypeOps a String eff
> teletypeHandler2 = CoOpHandler handleReturn handleOp
>  where
>    handleReturn :: a -> eff String
>    handleReturn _ = return ""
>
>    handleOp :: TeletypeCoOp (eff String) -> eff String
>    handleOp (GetLineOp cont) = do
>      lines <- get
>      case lines of
>        -- when input state is empty, feed an empty line
>        [] -> cont ""
>        (x:xs) -> do
>          put xs
>          y <- cont x
>          return $ x <> "\n" <> y
>
>    handleOp (PutLineOp x cont) = do
>      out <- cont ()
>      return $ x <> "\n" <> out

- We use `StateT [String] Identity` to provide the state effect to
  the teletype handlers, and then run it with some mock input.
- Notice the end result of the computation is a String, with the original
  return value discarded.

> hello53 :: StateT [String] Identity String
> hello53 = withOps stateTOps $
>   withCoOpHandler teletypeHandler2 hello51

> hello54 :: String
> hello54 = runIdentity $ evalStateT hello53 $
>   ["John", "Smith", "blah"]
>
> -- "Enter first name:\nJohn\nEnter last name:\nSmith\nHello, John Smith!\n"

- We can also introduce non-determinism by calling the continuation multiple
  times.
- We define another teletype handler, this time using state effect of `[[String]]`
  and return `[String]`
- Each `[String]` sub-array in the state represents multiple possible inputs
  when `getLine` is called.
- The result is list of possible console interactions when different inputs are
  returned from `getLine`.

> teletypeHandler3
>   :: forall a eff
>    . (EffConstraint (StateOps [[String]]) eff)
>   => CoOpHandler TeletypeOps a [String] eff
> teletypeHandler3 = CoOpHandler handleReturn handleOp
>  where
>    handleReturn :: a -> eff [String]
>    handleReturn _ = return [""]
>
>    handleOp :: TeletypeCoOp (eff [String]) -> eff [String]
>    handleOp (GetLineOp cont) = do
>     lines <- get
>     case lines of
>       [] -> cont ""
>       (xs:xss) -> do
>         put xss
>         yss <- traverse
>           (\x -> do
>             ys <- cont x
>             return $ fmap ((<>) (x <> "\n")) ys
>           ) xs
>         return $ join yss
>
>    handleOp (PutLineOp x cont) = do
>      xs <- cont ()
>      return $ fmap ((<>) (x <> "\n")) xs

> hello55 :: [String]
> hello55 = runIdentity $ evalStateT comp inputs
>  where
>   comp = withOps stateTOps $
>     withCoOpHandler teletypeHandler3 hello51
>
>   inputs =
>     [ ["Alice", "Bob", "Carl"]
>     , ["Jones", "Cooper"]
>     , ["Miller"]
>     ]
>
> -- [ "Enter first name:\nAlice\nEnter last name:\nJones\nHello, Alice Jones!\n"
> -- , "Enter first name:\nAlice\nEnter last name:\nCooper\nHello, Alice Cooper!\n"
> -- , "Enter first name:\nBob\nEnter last name:\nMiller\nHello, Bob Miller!\n"
> -- , "Enter first name:\nCarl\nEnter last name:\nHello, Carl !\n"
> -- ]

- The above example are a bit silly. The proper way to do this is to compose
  with other effects such as ambivalent, which will be discussed later.
- But it demonstrates the power of what a single co-op handler can do.

(To be continue...)