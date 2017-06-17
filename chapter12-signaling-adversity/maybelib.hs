-- maybelib.hs

module MaybeLib where

-- 1)
isJust :: Maybe a -> Bool
isJust option = case option of
  Just _  -> True
  Nothing -> False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2)
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee df f option = case option of
  Just value -> f value
  Nothing    -> df

-- 3)
fromMaybe :: a -> Maybe a -> a
fromMaybe df option = case option of
  Just value -> value
  Nothing    -> df

-- 4)
listToMaybe :: [a] -> Maybe a
listToMaybe list = case list of
  []  -> Nothing
  x:_ -> Just x

maybeToList :: Maybe a -> [a]
maybeToList option = case option of
  Just value -> [value]
  Nothing    -> []

-- 5)
catMaybes :: [Maybe a] -> [a]
catMaybes = map (fromMaybe undefined) . filter isJust

-- 6)
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where f option optionList = case (option, optionList) of
          (_, Nothing)            -> Nothing
          (Nothing, _)            -> Nothing
          (Just value, Just list) -> Just (value : list)
