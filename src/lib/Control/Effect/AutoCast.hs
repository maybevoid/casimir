{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Effect.AutoCast
  ( AutoCast (..)
  , Composable (..)
  , applyHandler
  , bindHandler
  )
where

import Control.Effect.Base (Union, NoEff)

import Control.Effect.Cast
  ( OpsCast (..)
  , Cast (..)
  )

import Control.Effect.Handler
  ( composeExactHandlers
  , composeHandlersWithCast
  , applyHandlerWithCast
  , bindHandlerWithCast
  )

import Control.Effect.Base
  ( Effect
  , EffOps (..)
  , Handler (..)
  , Computation (..)
  )

class (EffOps ops1, EffOps ops2) => AutoCast ops1 ops2 where
  autocast :: OpsCast ops1 ops2

class
  (EffOps ops1, EffOps ops2, EffOps ops3, EffOps handler1)
  => Composable ops1 ops2 ops3 handler1 where
  composeHandlers
    :: forall handler2 eff1 eff2 eff3 .
    ( EffOps handler2
    , Effect eff1
    , Effect eff2
    , Effect eff3
    )
    => Handler ops1 handler1 eff1 eff2
    -> Handler ops2 handler2 eff2 eff3
    -> Handler ops3 (Union handler1 handler2) eff1 eff3

instance (EffOps ops) => AutoCast ops ops where
  autocast = OpsCast Cast

instance {-# INCOHERENT #-}
  (EffOps ops)
  => AutoCast (Union NoEff ops) ops
  where
    autocast = OpsCast Cast

instance {-# INCOHERENT #-}
  (EffOps ops)
  => AutoCast (Union ops NoEff) ops
  where
    autocast = OpsCast Cast

instance {-# INCOHERENT #-}
  (EffOps ops)
  => AutoCast ops (Union NoEff ops)
  where
    autocast = OpsCast Cast

instance {-# INCOHERENT #-}
  (EffOps ops)
  => AutoCast ops (Union ops NoEff)
  where
    autocast = OpsCast Cast

instance {-# INCOHERENT #-}
  (EffOps ops1, EffOps ops2)
  => AutoCast (Union ops1 ops2) (Union ops2 ops1)
  where
    autocast = OpsCast Cast

-- distributive
-- ((ops1, ops2), ops3) :> (ops1, (ops2, ops3))
instance {-# INCOHERENT #-}
  (EffOps ops1, EffOps ops2, EffOps ops3)
  => AutoCast (Union (Union ops1 ops2) ops3) (Union ops1 (Union ops2 ops3))
  where
    autocast = OpsCast Cast

instance {-# OVERLAPPING #-}
  (EffOps ops1, EffOps ops2, EffOps handler1)
  => Composable
    ops1
    (Union handler1 ops2)
    (Union ops1 ops2)
    handler1
  where
    composeHandlers
      :: forall handler2 eff1 eff2 eff3 .
      ( EffOps handler2
      , Effect eff1
      , Effect eff2
      , Effect eff3
      )
      => Handler ops1 handler1 eff1 eff2
      -> Handler (Union handler1 ops2) handler2 eff2 eff3
      -> Handler (Union ops1 ops2) (Union handler1 handler2) eff1 eff3
    composeHandlers = composeExactHandlers

instance {-# INCOHERENT #-}
  (EffOps ops1, EffOps ops2, EffOps handler1)
  => Composable
    ops1
    ops2
    (Union ops1 ops2)
    handler1
  where
    composeHandlers
      :: forall handler2 eff1 eff2 eff3 .
      ( EffOps handler2
      , Effect eff1
      , Effect eff2
      , Effect eff3
      )
      => Handler ops1 handler1 eff1 eff2
      -> Handler ops2 handler2 eff2 eff3
      -> Handler (Union ops1 ops2) (Union handler1 handler2) eff1 eff3
    composeHandlers handler1 handler2 =
      composeHandlersWithCast
        @ops1 @ops2 @(Union ops1 ops2) @ops2
        handler1
        handler2
        (OpsCast Cast)
        (OpsCast Cast)

instance {-# INCOHERENT #-}
  (EffOps ops1, EffOps handler1)
  => Composable
    ops1
    handler1
    ops1
    handler1
  where
    composeHandlers
      :: forall handler2 eff1 eff2 eff3 .
      ( EffOps handler2
      , Effect eff1
      , Effect eff2
      , Effect eff3
      )
      => Handler ops1 handler1 eff1 eff2
      -> Handler handler1 handler2 eff2 eff3
      -> Handler ops1 (Union handler1 handler2) eff1 eff3
    composeHandlers handler1 handler2 =
      composeHandlersWithCast
        @ops1 @handler1 @ops1 @NoEff
        handler1
        handler2
        (OpsCast Cast)
        (OpsCast Cast)

instance {-# INCOHERENT #-}
  ( EffOps ops1
  , EffOps ops2
  , EffOps ops3
  , EffOps ops4
  , EffOps handler1
  , AutoCast (Union handler1 ops4) ops2
  , AutoCast ops3 (Union ops1 ops4)
  )
  => Composable
    ops1
    ops2
    ops3
    handler1
  where
    composeHandlers
      :: forall handler2 eff1 eff2 eff3 .
      ( EffOps handler2
      , Effect eff1
      , Effect eff2
      , Effect eff3
      )
      => Handler ops1 handler1 eff1 eff2
      -> Handler ops2 handler2 eff2 eff3
      -> Handler ops3 (Union handler1 handler2) eff1 eff3
    composeHandlers handler1 handler2 =
      composeHandlersWithCast
        @ops1 @ops2 @ops3 @ops4
        handler1
        handler2
        autocast
        autocast

applyHandler
  :: forall ops1 ops2 handler eff1 eff2 r .
  ( EffOps ops1
  , EffOps ops2
  , EffOps handler
  , Effect eff1
  , Effect eff2
  , OpsConstraint ops1 eff1
  , AutoCast (Union handler ops1) ops2
  )
  => Handler ops1 handler eff1 eff2
  -> Computation ops2 r eff2
  -> r eff1
applyHandler handler comp =
  applyHandlerWithCast handler comp autocast

bindHandler
  :: forall ops1 ops2 handler eff1 eff2 r .
  ( EffOps ops1
  , EffOps ops2
  , EffOps handler
  , Effect eff1
  , Effect eff2
  , AutoCast (Union handler ops1) ops2
  )
  => Handler ops1 handler eff1 eff2
  -> Computation ops2 r eff2
  -> Computation ops1 r eff1
bindHandler handler comp =
  bindHandlerWithCast handler comp autocast