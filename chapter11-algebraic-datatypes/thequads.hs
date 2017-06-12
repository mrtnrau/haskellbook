-- thequads.hs

module TheQuads where

data Quad =
    One
  | Two
  | Three
  | Four
  deriving (Show, Eq)

-- 1)
eQuad :: Either Quad Quad
eQuad = undefined
-- Arity: 4 + 4 = 8

-- 2)
prodQuad :: (Quad, Quad)
prodQuad = undefined
-- Arity: 4 * 4 = 16

-- 3)
funcQuad :: Quad -> Quad
funcQuad = undefined
-- Arity: 4 ^ 4 = 256

-- 4)
prodTBool :: (Bool, Bool, Bool)
prodTBool = undefined
-- Arity: 2 * 2 * 2 = 8

-- 5)
gTwo :: Bool -> Bool -> Bool
gTwo = undefined
-- Arity: 2 ^ 2 ^ 2 = 16

fTwo :: Bool -> Quad -> Quad
fTwo = undefined
-- Arity: 4 ^ 4 ^ 2 = 65536
