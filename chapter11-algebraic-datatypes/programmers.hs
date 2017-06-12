-- programmers.hs

module Programmers where

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Show, Eq)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | Purescript
  deriving (Show, Eq)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
             deriving (Show, Eq)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, Purescript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = x, lang = y } | x <- allOperatingSystems, y <- allLanguages ]
