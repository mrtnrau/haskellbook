-- understandingfolds.hs
module UnderstandingFolds where

-- 1) b) c)

-- 2)
-- foldl (flip (*)) 1 [1..3]
-- foldl (flip (*)) (1 * 1) [2,3]
-- foldl (flip (*)) (2 * 1) [3]
-- foldl (flip (*)) (3 * 2) []
-- 6

-- 3) c)

-- 4) a)

-- 5)
-- a) foldr (++) "" ["woot", "WOOT", "woot"]
-- b) foldr max ’ ’ "fear is the little death"
-- c) foldr (&&) True [False, True]
-- d) foldr (||) False [False, True]
-- e) foldl (\b a -> (++) b (show a)) "" [1..5]
-- f) foldr const 0 [1..5]
-- g) foldr const ' ' "tacos"
-- h) foldl (flip const) ' ' "burritos"
-- i) foldl (flip const) 0 [1..5]


