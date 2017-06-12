-- howdoesyourgardengrow.hs

module HowDoesYourGardenGrow where

data Flowertype =
    Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving Show

type Gardener = String

data Garden =
  Garden Gardener Flowertype
  deriving Show

-- in normal form
data Gardenia1 = Gardenia1 deriving Show
data Daisy1 = Daisy1 deriving Show
data Rose1 = Rose1 deriving Show
data Lilac1 = Lilac1 deriving Show

data GardenNormal =
    GardeniaGarden Gardener Gardenia1
  | DaisyGarden    Gardener Daisy1
  | RoseGarden     Gardener Rose1
  | LilacGarden    Gardener Lilac1
  deriving Show
