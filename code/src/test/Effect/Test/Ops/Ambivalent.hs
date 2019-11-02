
module Effect.Test.Ops.Ambivalent
where

import Control.Monad.Identity

import Test.Tasty
import Test.Tasty.HUnit

import Control.Effect.Implicit
import Control.Effect.Implicit.Free
import Control.Effect.Implicit.Free.Handler

ambivalentTests :: TestTree
ambivalentTests = testGroup "Ambivalent Tests"
  [ test1
  ]

data AmbEff a

data AmbOps a eff = AmbOps {
  selectOp :: [a] -> eff a
}

data AmbCoOp a r
  = SelectOp [a] (a -> r)

instance EffOps (AmbEff a) where
  type Operation (AmbEff a) = AmbOps a

instance EffCoOp (AmbEff a) where
  type CoOperation (AmbEff a) = AmbCoOp a

instance Functor (AmbCoOp a) where
  fmap f (SelectOp choice cont) = SelectOp choice (f . cont)

instance EffFunctor (AmbOps a) where
  effmap lifter ops = AmbOps {
    selectOp = \choice -> lifter $ selectOp ops choice
  }

instance FreeOps (AmbEff a) where
  mkFreeOps liftCoOp = AmbOps {
    selectOp = \choice -> liftCoOp $ SelectOp choice id
  }

instance ImplicitOps (AmbEff a) where
  type OpsConstraint (AmbEff a) eff = (?ambOps :: AmbOps a eff)

  withOps ops comp = let ?ambOps = ops in comp
  captureOps = ?ambOps

select
  :: forall a
   . [a]
  -> Eff (AmbEff a) a
select = selectOp captureOps

noAttack :: (Int, Int) -> (Int, Int) -> Bool
noAttack (x,y) (x',y') =
  (x /= x') && (y /= y') && ((abs (x - x')) /= (abs (y - y')))

availableMoves :: Int -> [(Int, Int)] -> [Int]
availableMoves x qs =
  filter (\y -> all (noAttack (x,y)) qs)
  [1..8]

solveQueen :: Eff (AmbEff Int) [(Int, Int)]
solveQueen = solveQueen' 1 []
 where
  solveQueen'
    :: Int
    -> [(Int, Int)]
    -> Eff (AmbEff Int) [(Int, Int)]
  solveQueen' x qs =
    if x == 9 then return qs else
    do
      y <- select $ availableMoves x qs
      solveQueen' (x+1) ((x, y) : qs)

dfsHandler
  :: forall a r eff
   . (Effect eff)
  => CoOpHandler (AmbEff a) r (Maybe r) eff
dfsHandler = CoOpHandler handleReturn handleCoOp
 where
  handleReturn x = return $ Just x

  handleCoOp (SelectOp choices cont) = tryChoices choices cont

  tryChoices [] _ = return Nothing
  tryChoices (x:xs) cont = do
    res <- cont x
    case res of
      Nothing -> tryChoices xs cont
      y@(Just _) -> return y

solution1 :: Maybe [(Int, Int)]
solution1 = runIdentity $ withCoOpHandler @ChurchMonad
  dfsHandler solveQueen

test1 :: TestTree
test1 = testCase "8 queen problem DFS test" $
  assertEqual "solution should be the following"
    (Just [(8,4),(7,2),(6,7),(5,3),(4,6),(3,8),(2,5),(1,1)])
    solution1
