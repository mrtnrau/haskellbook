-- eitherlib.hs

module EitherLib where

-- 1)
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f either errors = case either of
          Left error -> error : errors
          Right _    -> errors

-- 2)
rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f either values = case either of
          Left _      -> values
          Right value -> value : values

-- 3)
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where f either (errors, values) = case either of
          Left error  -> (error : errors, values)
          Right value -> (errors, value : values)

-- 4)
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f either = case either of
  Left _      -> Nothing
  Right value -> Just $ f value

-- 5)
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g either = case either of
  Left error  -> f error
  Right value -> g value

-- 6)
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)
