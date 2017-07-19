module HeavyLifting where

-- 1)
a = (+1) <$> read "[1]" :: [Int]

-- 2)
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3)
c = (*2) <$> (\x -> x - 2)

-- 4)
d = ((return '1' ++) . show) <$> (\x -> [x, 1..3])

-- 5)
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = (read :: String -> Integer) . ("123"++) . show <$> ioi
    in (*3) <$> changed

