## Implicit Effects: Algebraic Effects in Haskell using Implicit Parameters

[![Build Status](https://travis-ci.org/maybevoid/implicit-effects)](https://travis-ci.org/maybevoid/implicit-effects.svg?branch=master)

### Introduction

Implicit Effects is a new library for using algebraic effects in Haskell.
It uses the GHC language extension
[ImplicitParams](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-ImplicitParams)
to bind effect operations for a monad to the callee's context on call site.
This contrasts with the usual typeclass approach for implementing effects,
where instances of effect operations for a particular monad type is derived
globally with guaranteed uniqueness. Implicit Effects decouples the effects
definitions and interpretations from usage of effects on specific monad,
allowing computations to use implicit effects with _any_ monad, including
`Identity`, `IO`, MTL monads, free monads, or generic `forall m . (Monad m)`.

