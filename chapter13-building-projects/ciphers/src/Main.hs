module Main where

import Data.Char

type Shift = Int
type Text  = String
type Codeword = String

ceasar :: Shift -> Text -> Text
ceasar sh = map (shift sh)

unceasar :: Shift -> Text -> Text
unceasar i = ceasar (-i)

vigenere :: Codeword -> Text -> Text
vigenere = vigenere' id

unvigenere :: Codeword -> Text -> Text
unvigenere = vigenere' negate

vigenere' :: (Int -> Int) -> Codeword -> Text -> Text
vigenere' f code text = go code text
  where go _      []       = []
        go []     text     = go code text
        go code   (' ':ts) = ' ' : go code ts
        go (c:cs) (t:ts)   = shift (f $ ord c) t : go cs ts

shift :: Shift -> Char -> Char
shift sh char
  | isLower char = translate sh 'a' char
  | isUpper char = translate sh 'A' char
  | otherwise    = char
  where translate sh base char =
          chr $ ord base + mod (ord char - ord base + sh) 26

cipherWithCeasar :: (Shift -> Text -> Text) -> IO Text
cipherWithCeasar cipher = do
  putStrLn "Enter text to cipher: "
  text <- getLine
  putStrLn "Enter shift integer: "
  shift <- readLn :: IO Int
  return $ cipher shift text

main :: IO String
main = do
  putStrLn "Cipher or uncipher with Ceasar: c/u?"
  jn <- getChar
  case jn of
    'c' -> cipherWithCeasar ceasar
    _   -> cipherWithCeasar unceasar

