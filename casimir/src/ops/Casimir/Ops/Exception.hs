module Casimir.Ops.Exception
where

import Casimir.Base
import Casimir.Freer

data ExceptionTag
data ExceptionEff e

data ExceptionOps e m = ExceptionOps {
  raiseOp :: forall a . e -> m a
}

data ExceptionCoOp e a where
  RaiseOp :: e -> ExceptionCoOp e a

instance Effect (ExceptionEff e) where
  type Operation (ExceptionEff e) = ExceptionOps e

instance EffFunctor Lift (ExceptionOps e) where
  effmap (Lift lift) ops = ExceptionOps {
    raiseOp = \e -> lift $ raiseOp ops e
  }

instance Functor (ExceptionCoOp e) where
  fmap _ (RaiseOp e) = RaiseOp e

instance FreeOps (ExceptionOps e) where
  type CoOperation (ExceptionOps e) = ExceptionCoOp e

  mkFreeOps liftCoOp = ExceptionOps {
    raiseOp = \e -> liftCoOp $ RaiseOp e
  }

instance HasLabel (ExceptionOps e) where
  type GetLabel (ExceptionOps e) = Tag ExceptionTag

raise
  :: forall e a
   . e
  -> Eff '[ExceptionEff e] a
raise e = raiseOp captureOp e

-- mkExceptionCoOpHandler
--   :: forall m e a
--    . (Monad m)
--   => (e -> m a)
--   -> CoOpHandler (ExceptionEff e) a a m
-- mkExceptionCoOpHandler handleException =
--   CoOpHandler return $
--     \(RaiseOp e) -> handleException e

-- exceptionToEitherHandler
--   :: forall m e a
--    . (Monad m)
--   => CoOpHandler (ExceptionEff e) a (Either e a) m
-- exceptionToEitherHandler =
--   CoOpHandler handleReturn handleCoOp
--    where
--     handleReturn x = return $ Right x
--     handleCoOp (RaiseOp e) = return $ Left e

-- tryIo
--   :: forall e a .
--     (Ex.Exception e)
--   => IO a
--   -> Eff (IoEff ∪ (ExceptionEff e)) a
-- tryIo m = do
--   res <- liftIo $ Ex.try @e m
--   case res of
--     Left err -> raise err
--     Right val -> return val

-- try
--   :: forall free m e a
--    . ( Monad m
--      , FreeHandler free
--      )
--   => (OpsConstraint (ExceptionEff e) (free (ExceptionEff e) m)
--       => free (ExceptionEff e) m a)
--   -> (e -> m a)
--   -> m a
-- try comp handler1 = withCoOpHandler @free handler2 comp
--  where
--   handler2 :: CoOpHandler (ExceptionEff e) a a m
--   handler2 = CoOpHandler return $
--     \(RaiseOp e) -> handler1 e

-- tryFinally
--   :: forall free m e a
--    . ( FreeHandler free
--      , EffConstraint (ExceptionEff e) m
--      )
--   => ((OpsConstraint (ExceptionEff e) (free (ExceptionEff e) m))
--       => free (ExceptionEff e) m a)
--   -> (() -> m ())
--   -> m a
-- tryFinally comp handler1 =
--  do
--   res1' <- res1
--   handler1 ()
--   res2 res1'
--    where
--     res1 :: m (Either e a)
--     res1 = withCoOpHandler @free exceptionToEitherHandler comp

--     res2 :: Either e a -> m a
--     res2 res
--       = case res of
--         Left e -> raise e
--         Right x -> return x

-- tryComp
--   :: forall free m ops e a
--    . ( FreeHandler free
--      , Effects ops
--      , Effects ops
--      , EffConstraint ops m
--      , EffFunctor Lift (Operations ops)
--      )
--   => Computation Lift ((ExceptionEff e) ∪ ops) (Return a) m
--   -> (e -> m a)
--   -> m a
-- tryComp comp1 handler1 = handleFree handler2 comp2
--  where
--   comp2 :: free (ExceptionEff e) m a
--   comp2 = returnVal $ runComp comp1 freeLiftEff $
--     UnionOps freeOps $ effmap (Lift liftFree) captureOps

--   handler2 :: CoOpHandler (ExceptionEff e) a a m
--   handler2 = CoOpHandler return $
--     \(RaiseOp e) -> handler1 e

-- bracketComp
--   :: forall free m ops e a b
--    . ( FreeHandler free
--      , Effects ops
--      , Effects ops
--      , EffConstraint ops m
--      , EffFunctor Lift (Operations ops)
--      )
--   => BaseComputation ((ExceptionEff e) ∪ ops) (Return a) m          -- init
--   -> (a -> BaseComputation ((ExceptionEff e) ∪ ops) (Return ()) m)  -- cleanup
--   -> (a -> BaseComputation ((ExceptionEff e) ∪ ops) (Return b) m)   -- between
--   -> BaseComputation ((ExceptionEff e) ∪ ops) (Return b) m
-- bracketComp initComp cleanupComp betweenComp = Computation comp1
--  where
--   comp1
--     :: forall m2
--      . (Monad m2)
--     => Lift m m2
--     -> Operations ((ExceptionEff e) ∪ ops) m2
--     -> Return b m2
--   comp1 lift12 ops@(UnionOps eOps ops1) = Return comp5
--    where
--     comp2 :: m2 a
--     comp2 = returnVal $ runComp initComp lift12 ops

--     comp3 :: a -> m2 (Either e b)
--     comp3 x = handleFree @free
--       exceptionToEitherHandler $ returnVal $
--         runComp (betweenComp x)
--           (joinLift lift12 freeLiftEff) $
--           UnionOps freeOps $
--             effmap (Lift liftFree) ops1

--     comp4 :: a -> m2 ()
--     comp4 x = returnVal $ runComp (cleanupComp x) lift12 ops

--     comp5 :: m2 b
--     comp5 = do
--       x <- comp2
--       res <- comp3 x
--       comp4 x
--       case res of
--         Left e -> raiseOp eOps e >>= absurd
--         Right res' -> return res'
