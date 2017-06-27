module Person where

type Name = String
type Age = Integer

data Person =
  Person Name Age
  deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Show, Eq)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == ""            = Left NameEmpty
  | age <= 0              = Left AgeTooLow
  | otherwise             = Left $ PersonInvalidUnknown $
      "Name: " ++ show name ++ " Age: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Enter name: "
  name <- getLine
  putStrLn "Enter age: "
  age <- readLn :: IO Integer
  let failure invalid = "Person invalid: " ++ show invalid
  let success valid   = "Yay! " ++ show valid
  let result = either failure success $ mkPerson name age
  putStrLn result
