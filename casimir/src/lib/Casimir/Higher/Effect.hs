{-# LANGUAGE UndecidableSuperClasses #-}

module Casimir.Higher.Effect where

import Casimir.Higher.Params
import Casimir.Higher.ArgKind

import Data.Kind (Type)
import Data.Proxy
import Control.Monad.Trans.Reader
  ( ReaderT (..)
  , runReaderT
  )

import qualified Control.Monad.Trans.Reader as ReaderT

type Eff'
  (ops :: MonadPair -> Type)
  (m :: Type -> Type)
  = ReaderT (ops ('MonadPair m m)) m

type Eff ops a =
  forall ops2 m
   . ( Monad m
     , ParamConstraint ops ops2
     )
  => Eff' ops2 m a

type Eff1 ops a = Eff (Params '[ops]) a

type Ops ops =
  forall ops2 m
   . ( Monad m
     , ParamConstraint (Params '[ops]) ops2
     )
  => Eff' ops2 m (Operation' ops m)

captureOps
  :: forall ops
   . (HasLabel ops)
  => Ops ops
captureOps = getParam (Proxy @(GetLabel ops)) <$> ReaderT.ask

withOps
  :: forall ops m r
   . ( Monad m )
  => Operation' ops m
  -> ReaderT (Operation' ops m) m r
  -> m r
withOps ops cont = runReaderT cont ops

eff
  :: forall m a
   . m a
  -> (forall ops . Eff' ops m a)
eff = ReaderT . const

withCaptureOps
  :: forall ops r
   . (HasLabel ops)
  => (forall m
       . (Monad m)
       => ops ('MonadPair m m) -> m r)
  -> Eff1 ops r
withCaptureOps cont = do
  ops <- captureOps
  eff $ cont ops
