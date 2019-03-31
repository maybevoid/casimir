## Implicit Effects: Algebraic Effects in Haskell using Implicit Parameters

[![Build Status](https://travis-ci.org/maybevoid/implicit-effects.svg?branch=master)](https://travis-ci.org/maybevoid/implicit-effects)

  - [Haddock documentation](https://maybevoid.com/implicit-effects-haddock/)

### Introduction

`implicit-effects` is a new library for using algebraic effects in Haskell.
It uses the GHC language extension
[`ImplicitParams`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ImplicitParams)
to bind effect operations for a monad to the callee's context on call site.
This contrasts with the usual typeclass approach for implementing effects,
where instances of effect operations for a particular monad type is derived
globally with guaranteed uniqueness. `implicit-effects` decouple effect definitions
and interpretations from usage of effects on specific monad, allowing computations
to use implicit effects with _any_ monad, including `Identity`, `IO`, MTL monads,
free monads, or generic `forall m . (Monad m)`.

Although implicit parameters are used, `implicit-effects` hides the usage behind
a single typeclass `ImplicitOps`. Other than declaring new instances for
`ImplicitOps` when defining new effects, users are not exposed to implicit
parameters and can use the effect constraints just like regular typeclass
constraints. `implicit-effects` only requires free monad transformers for
advanced algebraic effects interpretations that require access to the
continuation. It is agnostic of the concrete free monad implementation,
allowing effect interpretation through any free monad transformer implementing
the `FreeEff` class. This allows users to switch between any free monad
variants with the most optimized performance without getting locked in to
any concrete implementation for their applications.

`implicit-effects` allows users to pay for the performance price of free
monads and full algebraic effects only when needed. For lightweight effect
interpretations that only wrap around other effects, e.g. `MonadTime` and
`Teletype`, users can define the effect operation handlers directly without
going through free monads. They can also make use of existing monads they
have defined for existing applications, such as MTL monad transformers stack,
and start with adding lightweight effects before moving to full algebraic
effects. Since effect interpretations are decoupled from computations, users
can mix and switch between lightweight interpretations, algebraic effect
handlers, and concrete monads with little to no change to their core
application logic.


### Work In Progress

`implicit-effects` is an _experimental_ effects library I developed after less
than a year study on algebraic effects. I am publishing `implicit-effects` to
share about different approaches I use to implement algebraic effects in
Haskell, which I think is worth _considering_ or _explored further_ by the
Haskell community. However considering this is my first serious personal Haskell
project, and that I lacks professional experience in developing production
quality Haskell applications, you may want to think twice before using
`implicit-effects` in any serious Haskell projects. (At least not yet)

### Operations and Co-Operations

An effect for `implicit-effects` is defined by declaring three datatypes and
implementing a few typeclass instances. We first need a dummy datatype as
effect _signature_, an _operation_ datatype for consumption by computations,
and a _co-operation_ datatype for interpretation of algebraic effects.

Consider a simple example for a time effect. Traditionally the operations for
time effect would be defined as a typeclass like `MonadTime`:

```haskell
class Monad m => MonadTime m where
  currentTime :: m UTCTime
```

In `implicit-effects`, we'd instead define the time effect as follow:

```haskell
data TimeEff
  -- empty body

data TimeOps eff = TimeOps {
  currentTimeOp :: eff UTCTime
}

data TimeCoOp r =
  CurrentTimeOp (UTCTime -> r)
  deriving (Functor)

instance EffSpec TimeEff where
  type Operation TimeEff = TimeOps
  type CoOperation TimeEff = TimeCoOp
```

We define a dummy `TimeEff` datatype with empty body for identifying the time
effect. We then define the operation type `TimeOps`, parameterized by a Monad
`eff`. (To make effect programming more friendly to beginners, in
`implicit-effects` we define `Effect` as a less scary type alias to `Monad` and
we name monadic type variables as `eff` instead of `m`) `TimeOps` will be bound
to implicit parameters later for used in computations. We also define the
co-operation `TimeCoOp` as the _dual_ to `TimeOps`.

`TimeCoOp` is just like the traditional payload functor for free monad, and can
be used for effect interpretation with a `CoOpHandler`. We will talk more about
co-operations in later sections on algebraic effects. For now we will first
focus on how `TimeOps` is used.

We then declare `TimeEff` as an instance of `EffSpec`. There are two type
families in `EffSpec` that defines what the operation and co-operation types
for an effect is, so we just put in `TimeOps` and `TimeCoOp` respectively.

We then have to define how `implicit-effects` can bind `TimeOps` to a specific
implicit parameter. This is done by implementing the `ImplicitOps` instance for
`TimeEff`:

```haskell
instance ImplicitOps TimeEff where
  type OpsConstraint TimeEff eff = (?timeOps :: TimeOps eff)

  withOps :: forall eff r . (Effect eff)
    => TimeOps eff
    -> ((OpsConstraint TimeEff eff) => r)
    -> r
  withOps ops comp = let ?timeOps = ops in comp

  captureOps :: forall eff
     . (Effect eff, OpsConstraint TimeEff eff)
    => TimeOps eff
  captureOps = ?timeOps
```

Using the GHC extension
[`ConstraintKinds`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ConstraintKinds),
the `OpsConstraint` type family in `ImplicitOps` defines the unique name of the
implicit parameter to bind the effect operation. Here we choose the name
`?timeOps`. Note that due to injectivity conditions imposed by
[`TypeFamilyDependencies`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#injective-type-families),
there should be naming conventions for the implicit parameters to avoid any
name clash which would result in compile time error.

The type signatures for `withOps` and `captureOps` are written here for
illustrative purpose. `withOps` implements how we can bind a `TimeOps` into
the `?timeOps` implicit parameter for any computation `comp` of any type `r`
that requires the implicit parameter `?timeOps` in its context. Conversely
`captureOps` is used to capture a `TimeOps` from the `?timeOps` implicit
parameter, if it is available in the current context. The implementation
for `withOps` and `captureOps` are simply usage of the relevant implicit
parameter expressions.

Note that with the types for `withOps` and `captureOps`, it is necessary for
the following law to hold for any non trivial effect operations:

```haskell
withOps ops captureOps = ops
```

Finally to make it easy for users to use `TimeEff`, we define the helper
function `currentTime` to access the `currentTimeOp` field of `TimeOps`
in the implicit parameter `?timeOps`:

```haskell
currentTime :: forall eff . (OpsConstraint TimeEff eff)
  => eff UTCTime
currentTime = currentTimeOp captureOps
```

With the above definitions, we can now define our first lightweight
interpretation of `TimeEff` under the `IO` monad:

```haskell
import Data.Time.Clock

ioTimeOps :: TimeOps IO
ioTimeOps = TimeOps getCurrentTime
```

For the trivial implementation, we just use the `getCurrentTime` function
from the `time` package to implement a `TimeOps` that can work only under
`IO`.

With our first interpretation of `TimeEff` implemented, we can write our
example app as follow that makes use of it:

```haskell
app :: forall eff
   . (EffConstraint (TimeEff ∪ IoEff) eff)
  => eff ()
app = do
  time <- currentTime
  liftIo $ putStrLn $ "the current time is " ++ show time

app' :: IO ()
app' = withOps (ioTimeOps ∪ ioOps) app
```

Above we can see a few new things introduced by `implicit-effects`. The
`EffConstraint` is a type alias that include both `Effect eff` and
`OpsConstraint` in a single constraint to reduce boilerplate. Without
it we could otherwise write
`(Effect eff, OpsConstraint (TimeEff ∪ IoEff) eff)`.

`(∪)` is the union type operator that we can use to combine multiple effect
operations. It is the type alias to `Union`, so if you can't figure how to
type "∪", you can write ``TimeEff `Union` IoEff`` or `Union TimeEff IoEff`
instead.

`IoEff` is one of the built in effects offered by `implicit-effects`. It
is the operation equivalent to the `MonadIO` typeclass, with a `liftIo`
operation. `ioOps` is the trivial instance for `IoOps IO`
(`Operation IoEff IO`) that offers `liftIo` under `IO`.

Our example application `app` is a generic computation that works under all
monad/effect `eff`. It has the constraint that requires both `TimeEff` and
`IoEff` be supported to run on the effect `eff`. By defining `app` generically,
we can run `app` on different effects later on, such as on a monad transformer
stack as the application grows, or use it with mock effects for testing.

We can bind the effect operations with `app` using `withOps` and get `app'`,
which works under `IO` as both `ioTimeOps` and `ioOps` works under `IO`. The
union operator `(∪)` at the term level is an alias to the `UnionOps`
constructor. By passing both operations as `ioTimeOps ∪ ioOps`, `withOps`
binds both constraints simultaneously with `app` and unify `eff` with `IO`.
With that we get to run our app in `main` just as if it has been written
to work with `IO` directly.

In the example above, we are just defining a simple time effect without
touching more advanced concepts such as effect interpretation. The main
takeaway is how simple it is to define an effect operation and use them.
As we dive deeper into `implicit-effects`, we will learn that even advanced
effects are defined similar to the above example.

One of the goals for `implicit-effects` is to avoid performance overhead
if possible. For the example `TimeEff`, since we are just letting `IO` do
bulk of the work, we don't need to pay for the performance cost of using
free monads or advanced constructs in `implicit-effects`. There is still
a little performance overhead for accessing the effect operations through
implicit parameters over typeclasses, but hopefully there is room for
optimization in GHC if enough people use implicit parameters.

We will also see later on more abstractions provided by `implicit-effects`,
and how the performance tradeoff may worth it when we use them to structure
more complex applications.

## To Be Continued..

I am still working on writing the documentation and tutorial for
`implicit-effects`. Thank you for taking the time to read until here.
In the meanwhile, you can refer to the
[Haddock documentation](https://maybevoid.com/implicit-effects-haddock/)
for `implicit-effects` to learn more.