# Implicit Effects: Algebraic Effects in Haskell using Implicit Parameters

[![Build Status](https://travis-ci.org/maybevoid/implicit-effects.svg?branch=master)](https://travis-ci.org/maybevoid/implicit-effects)

  - [Haddock documentation](https://maybevoid.com/implicit-effects-haddock/)

<i>
  `implicit-effects` is currently work in progress and will soon be announced
  publicly. The code is mostly ready but documentation is being worked on.
  Thank you for your understanding looking at the preview of this project.
</i>

## Introduction

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

In `implicit-effects`, we define the time effect instead as follow:

```haskell
data TimeEff
  -- empty body

data TimeOps eff = TimeOps {
  currentTimeOp :: eff UTCTime
}

instance EffOps TimeEff where
  type Operation TimeEff = TimeOps
```

We define a dummy `TimeEff` datatype with empty body for identifying the time
effect. We then define the operation type `TimeOps`, parameterized by a Monad
`eff`. (To make effect programming more friendly to beginners, in
`implicit-effects` we define `Effect` as a less scary type alias to `Monad` and
we name monadic type variables as `eff` instead of `m`) `TimeOps` will be bound
to implicit parameters later for used in computations.

We then declare `TimeEff` as an instance of `EffOps`. The typeclass requires us
to declare an `Operation` type for our effect `TimeEff`. Here we just put
`TimeOps` as the effect operation type.

We then have to define how `implicit-effects` can bind `TimeOps` to a specific
implicit parameter. This is done by implementing the `ImplicitOps` instance for
`TimeEff`:

```haskell
instance EffFunctor TimeEff where
  -- Required by ImplicitOps. We leave this undefined for now and will
  -- explain in the next section.
  effmap = undefined

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

There are a few new more things introduced in the example above.
`EffConstraint` is a type alias that include both `Effect eff` and
`OpsConstraint` in a single constraint to reduce boilerplate. Without
it we would otherwise write
`(Effect eff, OpsConstraint (TimeEff ∪ IoEff) eff)`.

`IoEff` is one of the built in effects offered by `implicit-effects`. It
is the operation equivalent to the `MonadIO` typeclass, with a `liftIo`
operation. `ioOps` is the trivial instance for `IoOps IO`
(`Operation IoEff IO`) that offers `liftIo` under `IO`.

```haskell
-- module Control.Effect.Implicit.Ops.Io

data IoOps eff = IoOps {
  liftIoOp :: forall a . IO a -> eff a
}

ioOps :: IoOps IO
ioOps = IoOps {
  liftIoOp = id
}
```

Our example application `app` is a generic computation that works under all
monad/effect `eff`. It has the constraint that requires both `TimeEff` and
`IoEff` be supported to run on the effect `eff`. By defining `app` generically,
we can run `app` on different effects later on, such as on a monad transformer
stack as the application grows, or use it with mock effects for testing.

`(∪)` is the union type operator that we can use to combine multiple effect
operations. It is the type alias to `Union`, so if you can't figure how to
type "∪", you can write ``TimeEff `Union` IoEff`` or `Union TimeEff IoEff`
instead.

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

## EffFunctor

Following the previous example, let's say we want to add a state effect to
store or update the fetched time. We can use `StateEff` provided by
`Control.Effect.Implicit.Ops.State`, which have the same interface as
`MonadState`.

```haskell
app2 :: forall eff
   . (EffConstraint (TimeEff ∪ IoEff  ∪ StateEff UTCTime) eff)
  => eff ()
app2 = do
  time1 <- get
  liftIo $ putStrLn $ "the previous recorded time is " ++ show time1

  time2 <- currentTime
  put time2
  liftIo $ putStrLn $ "the current time is " ++ show time2
```

With our updated app, we have to find a concrete monad for `eff` that
supports all three effect operations we need. For instance we may try to
implement `StateOps UTCTime IO` since we have already have `IO` instance
for the other two operations. Or we can use the algebraic effects approach
that we'll introduce in later section to implement state. But there is
already a well tested and high performance implementation for the state
effect, which is the `StateT` monad transformer provided by `mtl`, so why
not use that instead?

In fact `implicit-effects` provides the `stateTOps` instance for any
`StateT eff`:

```haskell
-- module Control.Effect.Implicit.Transform.State

stateTOps
  :: forall eff s
   . (Effect eff, MonadState s eff)
  => StateOps s eff
stateTOps = StateOps {
  getOp = get,
  putOp = put
}
```

`stateTOps` can provide state operation on any `MonadState` instance by just
delegating to `mtl`. With that we can for example run computations with
`StateEff` on any `StateT eff`. Given that our previous two effect operations
need to run on IO, it makes sense that we run our new app on `StateT IO`
instead. But to do that we need to make new operation handlers for
`StateOps (StateT IO)` and `IoOps (StateT IO)`.

`EffFunctor` is a typeclass that support lifting computations running on a
monad `eff` to a lifted monad `t eff`, similar to the `MonadTrans` class.
It is more general that it can lift computations through natural transformation
between any two effects `eff1` and `eff2`, without requiring `eff2` to be in
the form of `t eff1`.

```haskell
class EffFunctor (comp :: (Type -> Type) -> Type) where
  effmap :: forall eff1 eff2 .
    (Effect eff1, Effect eff2)
    => (eff1 ~> eff2) -- i.e. (forall x . eff1 x -> eff2 x)
    -> comp eff1
    -> comp eff2
```

Notice that `EffFunctor` is parameterized by a type variable `comp`, which
is parameterized by an monad `eff`. Operation handlers are one kind of
computation that can be an `EffFunctor`, but there are also more general
use of `EffFunctor` which we will learn later on.

`ImplicitOps` requires the `Operation` type of its effect to be an
`EffFunctor`. We will fill in our `EffFunctor` instance for `TimeOps`, which
we skipped in earlier section:

```haskell
instance EffFunctor TimeOps where
  effmap lifter ops = TimeOps {
    currentTimeOp = lifter $ currentTimeOp ops
  }
```

With that we can now to lift both `ioTimeOps` and `ioOps` to work on
`StateT UTCTime IO` using the `lift` method in `MonadTrans`.

```haskell
stateTIoTimeOps :: TimeOps (StateT UTCTime IO)
stateTIoTimeOps = effmap lift ioTimeOps

stateTIoOps :: TimeOps (StateT UTCTime IO)
stateTIoOps = effmap lift ioOps
```

With all 3 operation handlers available, we can now bind our application
and make it work on `StateT UTCTime IO`:

```haskell
app2' :: StateT UTCTime IO ()
app2' = withOps (stateTIoTimeOps ∪ stateTIoOps ∪ stateTOps) app2
```

We can run our `StatT` monad with the usual `evalStateT`, but say if we
need the initial state to be the time when the program starts, we'd once
again need to get the current time. Fortunately we can reuse the
`ioTimeOps` we defined earlier to do just that:

```haskell
app3 :: IO ()
app3 = withOps ioTimeOps $
  currentTime >>= evalStateT app2'
```

Given the popularity and stability of `mtl`, we can expect common use of
similar patterns as above to use monad transformers in conjunction with
`implicit-effects`. As such the helper function `withStateTAndOps` is
provided to help us do the same thing with much less boilerplate:

```haskell
app4 :: forall eff
   . (EffConstraint (TimeEff ∪ IoEff) eff)
  => eff ()
app4 = do
  start <- currentTime
  withStateTAndOps @(TimeEff ∪ IoEff) start app2
```

`withStateTAndOps` automatically captures any operation handler specified
in the type application, and automatically lift them to work on the lifted
`StateT` monad. It then runs the generic computation `app2` on
`StateT UTCTime eff` with the liften operation handlers, then finally run
`evalStateT` and return the result in the original monad `eff`.

Notice that both `app2` and `app4` are defined generically to work on any
monad `eff`. This means unlike `app3`, `app4` can run on deeper monad
transformer stacks with no modification required.

So far we are still working on replicating the familiar patterns of `mtl` and
make them work with `implicit-effects`. This may look redundant because if
that is all `implicit-effects` can do, it might as well be enough for us to
stick with just `mtl`. As we will learn in coming sections, the more
interesting things for `implicit-effects` is that we can also run algebraic
effects together with our regular `mtl` effects with minimal impact on
compatibility and performance.

## Computation

As we add more effects to our app, we may notice that binding an operation
handler to a computation function is kind of like function application
through implicit parameters. Since it is similar to function applications,
we can also do _partial_ application of effect operations to a computation
as well. Consider an example app:

```haskell
app1 :: forall eff
   . EffConstraint (TimeEff ∪ EnvEff AppConfig ∪ StateEff AppState) eff
  => eff ()
app1 = ...
```

Our app uses three effects, `TimeEff` for getting current time, `EnvEff` to
reading the app config, and `StateEff` for storing states. Our app is free
of `IoEff`, which makes it much easier to test with mock effects.

It may be cumbersome if we only bind all effect operations in our main program.
For `EnvEff` and `StateEff`, we may still need to write some code to get the
environment or initialize state. But for `TimeEff`, we pretty much know that
our real app is going to use some IO to get the time. So why not bind it with
`ioTimeOps` first:

```haskell
app2 :: EffConstraint (EnvEff AppConfig ∪ StateEff AppState) IO
  => IO ()
app2 = withOps ioTimeOps app1
```

By binding `ioTimeOps` with app1, we also unintentionally unified the effect
variable `eff` with `IO`, since `ioTimeOps` is defined to run directly with
`IO`. However with that binding we now have a problem: we now have to provide
`EnvEff` and `StateEff` ops handlers that can run on `IO`, not
`ReaderT AppConfig IO` or `StateT AppState IO` or
`ReaderT AppConfig StateT AppState IO`. Because of that, we can't reuse our
`readerTOps` and `stateTOps` that are defined to work only under lifted monads.

The problem is partial application of operation handlers have too eagerly bind
the concrete monad for the entire computation. Instead we need some way to
lazily hold on to both `ioTimeOps` and `app1`, and lift them to some other
monad at later time is necessary. Fortunately `implicit-effects` solves this
by providing the `Computation` datatype:

```haskell
app3 :: forall eff . (Effect eff)
  => Computation
       (TimeEff ∪ EnvEff AppConfig ∪ StateEff AppState)
       (Return ())
       eff
app3 = genericReturn app1
```

The `Computation` type have the following signature:

```haskell
newtype Computation ops comp eff1 = Computation {
  runComp :: forall eff2 .
    ( ImplicitOps ops
    , Effect eff1
    , Effect eff2
    )
    => LiftEff eff1 eff2
    -> Operation ops eff2
    -> comp eff2
}
```

The detailed machinery is not too important at this moment, but the gist is
that `Computation` provides a _liftable_ computation wrapper around generic
computation functions. The first type argument is the effect signatures of
the required effect operations. The second type argument is the computation
type parameterized by an effect `eff`. In normal function computations we
use the `Return` wrapper type which is defined as:

```haskell
newtype Return a eff = Return {
  returnVal :: eff a
}
```

Finally the third type argument to `Computation` is the base monad that is can
run on, as well as the base monad of operation handlers that it can accept.

The `genericReturn` helper is provided by `implicit-effects` to wrap a plain
function computation into a `Computation`:

```haskell
genericReturn :: forall ops a . (ImplicitOps ops)
  => (forall eff . (EffConstraint ops eff)
      => eff a)
  -> (forall eff . (Effect eff)
      => Computation ops (Return a) eff)
```

Other than `Return` computation for plain functions, effect `Operation`s are
also computations since they are also parameterized by a monad type.


```haskell
ioTimeHandler :: Computation NoEff TimeOps IO
ioTimeHandler = baseOpsHandler ioTimeOps
```

The `baseOpsHandler` helper is provided by `implicit-effects` to wrap an
operation handler to a computation.

```haskell
baseOpsHandler :: forall handler eff
   . (ImplicitOps handler, Effect eff)
  => Operation handler eff
  -> Computation NoEff (Operation handler) eff
```

`NoEff` is the empty effect signature, indicating that the operation handler
computation defined does not depend on other effects. As you may have guess,
soon after this we will talk about operation handlers that depend on other
effects.

Given we have an ops handler computation and a return computation, we can
bind the two together as follow:

```haskell
app4 :: Computation
         (EnvEff AppConfig ∪ StateEff AppState)
         (Return ())
         IO
app4 = bindOpsHandlerWithCast
  cast cast
  ioTimeHandler app3
```

`bindOpsHandlerWithCast` is a helper function provided by `implicit-effects` that
binds an ops handler to a computation, at the same time perform safe "casting"
of the effect union constraints to a suitable target type. We will go into
details on ops casting in later sections, but the gist of it is that without it
we'd have to write something like:

```haskell
app4' :: Computation
         (NoEff ∪ EnvEff AppConfig ∪ StateEff AppState)
         (Return ())
         IO
app4' = bindExactOpsHandler ioTimeHandler app3
```

Notice that `NoEff` is appended to the front of `app4'`'s effect dependencies,
as the precise version `bindExactOpsHandler` merges the effect operations
required by both `ioTimeHandler` and `app3`. But through some Haskell hacks
called _constraint casting_, we can statically show proofs to Haskell
that `NoEff` can be _eliminated_ since it has a trivial constraint that can
always be satisfied. We'll leave further explanation to the next sections
and continue with our example.

The nice thing about `Computation` is that we can partially apply ops handlers
as many times as we need, as long as the ops handlers can run on either the
current monad _or_ a lifted monad. With that we can for example further bind
our app with a `StateT` handler:

```haskell
app5 :: Computation
         (EnvEff AppConfig ∪ StateEff AppState)
         (Return ())
         StateT AppState IO
app5 = liftComputation stateTLiftEff app4

app6 :: Computation
         (EnvEff AppConfig)
         (Return ())
         StateT AppState IO
app6 = bindOpsHandlerWithCast
  cast cast
  stateTHandler app5
```

The `liftComputation` function is used to lift a computation to run on a lifted
monad, by providing it a `LiftEff` object. `LiftEff` is an opaque object that
can be used to apply `effmap` to an `EffFunctor`, but with the optimization
that if it is an identity `idLift`, it just skips the `effmap` and returns the
original `EffFunctor`.

We first lift our app to work on `StateT AppState IO`, and then use
`bindOpsHandlerWithCast` to bind it with `stateTHandler`, which is provided
as the `Computation` version of `stateTOps`. Also notice that
`bindOpsHandlerWithCast` allows _reordering_ of effect operations, so
we can still bind it even though `StateEff` appears in the _last_ position
in our original type for `app3`.

We can then similarly continue with binding our reader ops with `ReaderT`:

```haskell
app7 :: Computation
         NoEff
         (Return ())
         ReaderT AppConfig StateT AppState IO
app7 = bindOpsHandlerWithCast
  cast cast
  readerTHandler $
  liftComputation readerTLiftEff app6
```

Now we have "fully applied" the effect operations of a computation, making it
requiring only `NoEff`. With that we can use `execComp` to run our computation
and get back the underlying function:

```haskell
app8 :: ReaderT AppConfig StateT AppState IO ()
app8 = execComp app7
```

At this stage we can then execute our monad transformer stack as usually, and
finally get back `IO ()`. In practice, `app4` through `app8` can slowly
be transformed in different parts of our codebase. We can essentially
fully utilize the functional programming style and doing partial applications
of named parameters to our computations like regular functions.

## Pipeline

At this point you may be suspicious of the above roundabout way of using
`implicit-effects` just to get back our favorite monad transformer stacks.
There is actually even simplified abstractions provided by `implicit-effects`
that lets us use `mtl` without touching the monad transformers like  `ReaderT`
and `StateT`.

```haskell
app1 :: Computation
         (EnvEff AppConfig ∪ StateEff AppState)
         (Return ())
         IO
app1 = ...

initialAppState :: AppState
initialAppState = ...

app2 :: Computation
         (EnvEff AppConfig)
         (Return ())
         IO
app2 = runPipelineWithCast
  cast cast
  (stateTPipeline initialAppState)
  app1
```

A `Pipeline` is simply generic functions that transforms computations, wrapped
in a `newtype`. `stateTPipeline` is one of the pipelines provided by
`implicit-effects` that given an initial state, it performs transformation
that _removes_ the `StateEff` operation from a `Computation` without changing
the monad type. Here again we have to use some ops casting magic with
`runPipelineWithCast` that reorder the effect operations of the original
computation, and remove the `NoEff` noise from the result computation.

Notice here `stateTPipeline` completely encapsulates the fact that we are using
`StateT` underneath. As far as the computation concerns, we can swap in
different pipelines that implement `StateEff`, with performance characteristics
being the only difference.

...

## To Be Continued..

I am still working on writing the documentation and tutorial for
`implicit-effects`. Thank you for taking the time to read until here.
In the meanwhile, you can refer to the
[Haddock documentation](https://maybevoid.com/implicit-effects-haddock/)
for `implicit-effects` to learn more.

You can also look at the [effect operation unit tests](src/test/Effect/Test/Ops)
which has some example use of `implicit-effects`.


## References

  - [Effects bibliography](https://github.com/yallop/effects-bibliography), Jeremy Yallop
  - [Programming with Algebraic Effects and Handlers](http://math.andrej.com/wp-content/uploads/2012/03/eff.pdf),
    Andrej Bauer and Matija Pretnar
  - [An Introduction to Algebraic Effects and Handlers](http://www.eff-lang.org/handlers-tutorial.pdf),
    Matija Pretnar
  - [What is algebraic about algebraic effects and handlers?](https://arxiv.org/pdf/1807.05923.pdf),
    Andrej Bauer
  - [Freer monads, more extensible effects](http://okmij.org/ftp/Haskell/extensible/more.pdf),
    Oleg Kiselyov and Hiromi Ishii
  - [Shallow Effect Handlers](http://homepages.inf.ed.ac.uk/slindley/papers/shallow-extended.pdf),
    Daniel Hillerström and Sam Lindley

  - [Freer Monads: Too Fast, Too Free](https://reasonablypolymorphic.com/blog/too-fast-too-free/),
    Sandy Maguire
  - [Free Monad Benchmarks](https://github.com/feuerbach/freemonad-benchmark), Roman Cheplyaka