
module Benchmark.State.Base
where

import Control.Monad

import Control.Effect

newtype CoState s eff a = CoState (s -> eff a)

stateBaseFunc
  :: forall eff
   . (Effect eff, OpsConstraint (StateEff Int) eff)
  => eff ()
stateBaseFunc = forM_ [0..500] $ \i ->
 do
  s <- get
  let s' = s + i
  put s'

stateBaseComp :: GenericReturn (StateEff Int) ()
stateBaseComp = genericReturn stateBaseFunc

runCoState
  :: forall s eff . (Effect eff)
  => s
  -> (forall a . CoState s eff a -> eff a)
runCoState i (CoState cont) = cont i

stateCoOpHandler
  :: forall eff s a .
  (Effect eff)
  => OpsHandler (StateEff s) a (CoState s eff a) eff
stateCoOpHandler = OpsHandler handleReturn' handleOps'
 where
  {-# INLINE handleReturn' #-}
  handleReturn' :: a -> eff (CoState s eff a)
  handleReturn' x = return $ CoState $ \_ -> return x

  {-# INLINE handleOps' #-}
  handleOps' :: StateCoOp s (eff (CoState s eff a)) -> eff (CoState s eff a)
  handleOps' (GetOp cont1) = return $ CoState $
    \s ->
     do
      (CoState cont2) <- cont1 s
      cont2 s
  handleOps' (PutOp s cont1) = return $ CoState $
    \_ ->
     do
      (CoState cont2) <- cont1 ()
      cont2 s
