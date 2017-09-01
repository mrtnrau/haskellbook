module RandomExample2 where


import           Control.Applicative       (liftA3)
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           RandomExample
import           System.Random


rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)


rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))


rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie


nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie


rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty = rollsToGetN 20


-- 1)
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit = go 0 0
  where
  go sum count gen
    | sum >= limit = count
    | otherwise    = go (sum + die) (count + 1) nextGen
                     where
                     (die, nextGen) = randomR (1, 6) gen


-- 2)
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit = go 0 0 []
  where
  go sum count rolled gen
    | sum >= limit = (count, rolled)
    | otherwise    =
      go (sum + die) (count + 1) (rolled ++ [intToDie die]) nextGen
      where
      (die, nextGen) = randomR (1, 6) gen
