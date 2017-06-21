module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList =
  WordList [String]
  deriving (Show, Eq)

data Puzzle =
  Puzzle String [Maybe Char] String Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed nguesses) =
    intersperse ' ' (fmap (fromMaybe '_') discovered) ++ " Guessed so far: " ++ guessed ++ ", #Guesses: " ++ show nguesses

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) [] numberOfGuesses

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) char = char `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) char = char `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s no) c = Puzzle word newFilledInSoFar (c:s) no
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar = zipWith (zipper c) word filledInSoFar

adjustGuesses :: Puzzle -> Char -> Puzzle
adjustGuesses puzzle@(Puzzle word filledInSoFar guessed no) char =
  if charInWord puzzle char
  then Puzzle word filledInSoFar guessed no
  else Puzzle word filledInSoFar guessed (no - 1)

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
gameOver (Puzzle wordToGuess _ _ no) =
  if no == 0
  then do
    putStrLn "You lose!"
    putStrLn $ "The word was " ++ wordToGuess
    exitSuccess
  else
    return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar
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
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your gess must be a single character."

minWordLength :: Int
minWordLength = 5

numberOfGuesses :: Int
numberOfGuesses = 7

maxWordLength :: Int
maxWordLength = 7

allWords :: IO WordList
allWords = do
  dict <- readFile "data/british-english"
  return $ WordList $ lines dict

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList $ filter gamelength aw
  where gamelength w =
          let l = length w in
          l >= minWordLength && l <= maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
