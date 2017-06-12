-- vehicles.hs

module Vehicle where

data Price =
  Price Integer
  deriving (Show, Eq)

data Size =
  Size Integer
  deriving (Show, Eq)

data Manufacturer =
    Mini
  | Mazda
  | Tata
  deriving (Show, Eq)

data Airline =
    PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Show, Eq)

data Vehicle =
    Car Manufacturer Price
  | Plane Airline Size
  deriving (Show, Eq)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 700)

-- 1) :t myCar -- Vehicle
-- 2)
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3)
getManu :: Vehicle -> Maybe Manufacturer
getManu (Car manufacturer _) = Just manufacturer
getManu _                    = Nothing

-- Cardinality
-- 1) 1
-- 2) 3
-- 3) 32768 * 2
-- 4) 9223372036854775808 * 2 Int, infinite Integer
-- 5) 2 ^ 8
