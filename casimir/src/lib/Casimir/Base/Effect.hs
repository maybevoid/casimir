module Casimir.Base.Effect where

import Casimir.Base.Params

import Data.Proxy (Proxy (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..)
  , runReaderT
  )

import qualified Control.Monad.Trans.Reader as ReaderT

type Eff' ops m = ReaderT (ops m) m

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
  => Eff' ops2 m (ops m)

captureOps
  :: forall ops
   . (HasLabel ops)
  => Ops ops
captureOps = getParam (Proxy @(GetLabel ops)) <$> ReaderT.ask

withOps
  :: forall ops m r
   . ( Monad m )
  => ops m
  -> ReaderT (ops m) m r
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
  => (forall m . ops m -> m r)
  -> Eff1 ops r
withCaptureOps cont = do
  ops <- captureOps
  eff $ cont ops
