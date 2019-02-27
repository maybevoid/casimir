{-# LANGUAGE DeriveFunctor #-}

module Control.Effect.OpsHandler

where

import Control.Effect.Class
import Control.Monad.Identity

data OpsHandler f eff a r = OpsHandler {
  handleReturn :: a -> eff r,
  handleOps :: f (eff r) -> eff r
}

toOpsArg
  :: forall f eff a r .
  ( Functor f
  , Effect eff
  )
  => OpsHandler f eff a r
  -> f (eff a)
  -> f (eff r)
toOpsArg handler ops = fmap mapper ops
  where
    mapper :: eff a -> eff r
    mapper mx = mx >>= (handleReturn handler)

data OpsMonad f eff a = OpsMonad {
  runOpsMonad :: forall r . OpsHandler f eff a r -> eff r
}

mapOpsMonad
  :: forall f eff a b .
  ( Functor f
  , Effect eff
  )
  => (a -> b)
  -> OpsMonad f eff a
  -> OpsMonad f eff b
mapOpsMonad f (OpsMonad m1) = OpsMonad m2
 where
  m2 :: forall r . OpsHandler f eff b r -> eff r
  m2 handler = m1 $ OpsHandler {
    handleReturn = \x -> handleReturn handler (f x),
    handleOps = handleOps handler
  }

bindOpsMonad
  :: forall f eff a b .
  ( Functor f
  , Effect eff
  )
  => OpsMonad f eff a
  -> (a -> OpsMonad f eff b)
  -> OpsMonad f eff b
bindOpsMonad (OpsMonad m1) cont1 = OpsMonad m2
 where
  m2 :: forall r . OpsHandler f eff b r -> eff r
  m2 handler1 = m1 handler2
   where
    handler2 :: OpsHandler f eff a r
    handler2 = OpsHandler {
      handleReturn = \x -> runOpsMonad (cont1 x) handler1,
      handleOps = handleOps handler1
    }

liftPure
  :: forall f eff a .
  ( Functor f
  , Effect eff
  )
  => a
  -> OpsMonad f eff a
liftPure x = OpsMonad $ \handler -> handleReturn handler x

liftReturn
  :: forall f eff a .
  ( Functor f
  , Effect eff
  )
  => eff a
  -> OpsMonad f eff a
liftReturn mx = OpsMonad $ \handler -> do
  x <- mx
  handleReturn handler x

liftOps
  :: forall f eff a .
  ( Functor f
  , Effect eff
  )
  => f (eff a)
  -> OpsMonad f eff a
liftOps ops = OpsMonad $ cont
 where
  cont :: forall r . OpsHandler f eff a r -> eff r
  cont handler = handleOps handler $ fmap mapper ops
   where
    mapper :: eff a -> eff r
    mapper mx = do
      x <- mx
      handleReturn handler x

instance (Monad eff, Functor f) => Functor (OpsMonad f eff) where
  fmap = mapOpsMonad

instance (Monad eff, Functor f) => Applicative (OpsMonad f eff) where
  pure = liftPure
  mf <*> mx = do
    f <- mf
    x <- mx
    return $ f x

instance (Monad eff, Functor f) => Monad (OpsMonad f eff) where
  (>>=) = bindOpsMonad


data Decide a = Decide (Bool -> a)
  deriving (Functor)

trueHandler
  :: forall eff .
  (Effect eff)
  => OpsHandler Decide eff Int String
trueHandler = OpsHandler {
  handleReturn = return . show,
  handleOps = \(Decide cont) -> cont True
}

nonDetHandler
  :: forall eff .
  (Effect eff)
  => OpsHandler Decide eff Int [Int]
nonDetHandler = OpsHandler {
  handleReturn = \x -> return [x],
  handleOps = \(Decide cont) -> do
    res1 <- cont True
    res2 <- cont False
    return $ res1 ++ res2
}

testDecide :: OpsMonad Decide IO Int
testDecide = do
  a <- liftOps (Decide return)
  liftReturn $ putStrLn $ "a: " ++ (show a)
  b <- liftOps (Decide return)
  liftReturn $ putStrLn $ "b: " ++ (show b)
  return $ if a
    then if b then 1 else 2
    else if b then 3 else 4

decideRes :: IO [Int]
decideRes = runOpsMonad testDecide nonDetHandler
