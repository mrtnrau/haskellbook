-- squarecube.hs
module SquareCube where

mySqr = [x^2 | x <- [1..5]]
myCube = [x^3 | x <- [1..5]]

-- 1)
myTuples1 = [(x, y) | x <- mySqr, y <- myCube]

-- 2)
myTuples2 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3)
lengthMT1 = length myTuples1
lengthMT2 = length myTuples2
