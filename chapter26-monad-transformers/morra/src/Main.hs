{-# LANGUAGE QuasiQuotes #-}


module Main where


import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Bool                 (bool)
import           Data.List                 (isPrefixOf)
import           Data.Maybe                (catMaybes)
import           Data.String.Here
import           System.Random

--------------------------------------------------------------------------
-- Formatting terminal output

scoreTemplate :: Score -> String
scoreTemplate (Score (o, e)) = [i|
  Score:
      Odd    ${o}
      Even   ${e}|]


roundTemplate :: Round -> String
roundTemplate (Round (o, e)) = [i|
  Round:
      Odd    ${o}
      Even   ${e}|]


gameTemplate :: Game -> String
gameTemplate (Game s l m po pe (r:rs)) = [i|
+----------+ ${l} +----------+
  Mode: ${m} ${po} ${pe}
${r}
${s}
+----------+ ${l} +----------+|]


blankTemplate :: String
blankTemplate = replicate 20 '\n'


handTemplate :: String
handTemplate = [here|
Please Enter: '1' or '2':
>|]


quitTemplate :: String
quitTemplate = [here|
Do you want to quit? 'y'/'n'
>|]


modeTemplate :: String
modeTemplate = [here|
Which Mode do you want to play?
    PvP:   1
    PvAI:  2
>|]


rules :: String
rules = [here|
+-----------+ Morra +-----------+
       Player vs Player
       Odd    vs Even

Both show one or two fingers
If the number of fingers is even
          Even wins!
If the number of fingers is odd
          Odd wins!
+-----------+ Begin +-----------+|]

--------------------------------------------------------------------------
-- Types

newtype Hand = Hand Int
  deriving (Show, Eq)


data Player
  = Human
  | AI
  deriving Show


data Mode
  = PvP
  | PvAI
  deriving (Show, Eq)


newtype Score = Score (Int, Int)

instance Show Score where
  show = scoreTemplate


newtype Round = Round (Hand, Hand)

instance Show Round where
  show = roundTemplate


type Laps = Int

data Game = Game Score Laps Mode Player Player [Round]

instance Show Game where
  show = gameTemplate

--------------------------------------------------------------------------
-- Game Logic

playHand :: Player -> [Hand] -> IO Hand
playHand player past = case player of
  Human -> readHand
  AI    -> smartHand sub1 sub2 past -- no smart AI? -> substitute randomHand
  where (sub1, sub2) = genSubsequences past


readHand :: IO Hand
readHand = do
  putStr handTemplate
  c <- getChar
  putStrLn ""
  case c of
    '1' -> return $ Hand 1
    '2' -> return $ Hand 2
    _   -> readHand

-- Some... AI...
-- countSubsequence, take', pastRounds, smartHand

countSubsequence :: Eq a => [a] -> [a] -> Int
countSubsequence [] _ = 0
countSubsequence _ [] = 0
countSubsequence prefix list@(x:xs) =
  bool next (1+next) (prefix `isPrefixOf` list)
  where next = countSubsequence prefix xs


take' :: Int -> [a] -> Maybe [a]
take' 0 _      = Just []
take' i []     = Nothing
take' i (x:xs) = (x:) <$> take' (i-1) xs


-- takes the hand history of one player
-- generates the 1..4 last played hands
--      new -> old
-- e.g. [1,2,1,2,1]
--      [[1], [1,2], [1,2,1], [1,2,1,2]]
-- expands those with the next hand to be played
-- either Hand 1 or Hand 2
-- e.g.   [[1,1], [1,1,2], [1,1,2,1], [1,1,2,1,2]]
--        [[2,1], [2,1,2], [2,1,2,1], [2,1,2,1,2]]
-- these subsequences will be counted in the hand history
genSubsequences :: [Hand] -> ([[Hand]], [[Hand]])
genSubsequences hands = (fmap (Hand 1:) history, fmap (Hand 2:) history)
  where history = catMaybes $ flip take' hands <$> [1..4]


-- takes the 1..4 last played hands
-- once expanded with Hand 1
-- once expanded with Hand 2
-- and the history of played hands
-- generates the hand to be played next depending on how often
-- the expanded respective subsequences occurred in the history of
-- played hands
smartHand :: [[Hand]] -> [[Hand]] -> [Hand] -> IO Hand
smartHand sub1 sub2 pastRounds =
  case compare ones twos of
    LT -> return $ Hand 2
    EQ -> randomHand
    GT -> return $ Hand 1
  where f subs = flip countSubsequence pastRounds <$> subs
        (ones, twos) = (sum $ f sub1, sum $ f sub2)


randomHand :: IO Hand
randomHand = do
  gen <- newStdGen
  let (a, _) = randomR (1, 2) gen
  return $ Hand a


updateScore :: Round -> Score -> Score
updateScore (Round (Hand o, Hand e)) (Score (os, es)) = Score $
  bool (os+1, es) (os, es+1) (even $ o + e)


tellFinish :: Game -> IO ()
tellFinish (Game (Score (o, e)) _ _ _ _ _) =
  case compare o e of
    LT -> putStrLn [i|Even Player won! ${o}:${e}|]
    EQ -> putStrLn [i|Tie! ${o}:${e}|]
    GT -> putStrLn [i|Odd Player won! ${o}:${e}|]


quit :: IO Bool
quit = do
  putStr quitTemplate
  i <- getChar
  putStrLn ""
  case i of
    'y' -> return True
    'n' -> return False
    _   -> quit


mode :: IO (Mode, Player, Player)
mode = do
  putStr modeTemplate
  m <- getChar
  putStrLn ""
  case m of
    '1' -> return (PvP, Human, Human)
    '2' -> return (PvAI, Human, AI)
    _   -> mode


gameLoop :: StateT Game IO ()
gameLoop = do
  Game score laps m op ep rounds <- get
  let opast = (\(Round oe) -> fst oe) <$> rounds
      epast = (\(Round oe) -> snd oe) <$> rounds
  oh <- liftIO $ playHand op epast -- the opponents past
  bool (liftIO $ putStr "") (liftIO $ putStr blankTemplate) (m == PvP)
  eh <- liftIO $ playHand ep opast -- the opponents past
  let round  = Round (oh, eh)
      score' = updateScore round score
      laps'  = laps + 1
      game'  = Game score' laps' m op ep (round : rounds)
  liftIO $ print game'
  put game'
  finish <- liftIO quit
  bool gameLoop (return ()) finish


main :: IO ()
main = do
  putStrLn rules
  (m, p1, p2) <- mode
  let game = Game (Score (0,0)) 0 m p1 p2 []
      run  = runStateT gameLoop
  (_, finish) <- run game
  tellFinish finish
  return ()
