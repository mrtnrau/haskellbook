module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- type and data declarations
newtype WordList =
  WordList [String]
  deriving (Show, Eq)

type WOrd  = String
type Found = [Maybe Char]
type Guess = String
type Try   = Int

data Puzzle =
  Puzzle WOrd Found Guess Try

instance Show Puzzle where
  show (Puzzle _ found guess try) =
    intersperse ' ' (fmap (fromMaybe '_') found)
    ++ " Guessed so far: " ++ guess
    ++ ", Remaining tries: " ++ show try

-- constants
minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 7

numberOfTries :: Int
numberOfTries = 7

-- setup random word and a fresh hangman puzzle
allWords :: IO WordList
allWords = do
  dict <- readFile "data/british-english"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gamelength aw)
  where gamelength w =
          let l = length w in
          l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

gameWord :: IO String
gameWord = gameWords >>= randomWord

freshPuzzle :: WOrd -> Puzzle
freshPuzzle word =
  Puzzle word (map (const Nothing) word) [] numberOfTries

-- game logic
charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) char = char `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guess _) char = char `elem` guess

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word found guess try) char =
  Puzzle word found' (char:guess) try
  where zipper guess wordChar guessChar =
          if wordChar == guess
          then Just wordChar
          else guessChar
        found' = zipWith (zipper char) word found

adjustGuesses :: Puzzle -> Char -> Puzzle
adjustGuesses puzzle@(Puzzle word found guess try) char =
  if charInWord puzzle char
  then Puzzle word found guess try
  else Puzzle word found guess (try - 1)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly."
      return $ adjustGuesses (fillInCharacter puzzle guess) guess
    (False, _) -> do
      putStrLn "This character was not in the word. Try again!"
      return $ adjustGuesses (fillInCharacter puzzle guess) guess

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ try) =
  if try == 0
  then do
    putStrLn "You lose!"
    putStrLn $ "The word was " ++ wordToGuess
    exitSuccess
  else
    return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ found _ _) =
  if all isJust found
  then do
    putStrLn "You win!"
    exitSuccess
  else
    return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [char] -> handleGuess puzzle char >>= runGame
    _   -> putStrLn "Your guess must be a single character."

main :: IO ()
main = do
  word <- gameWord
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
