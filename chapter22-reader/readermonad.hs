module ReaderMonad where


import           Control.Applicative (liftA2)


newtype HumanName = HumanName String
  deriving (Eq, Show)


newtype DogName = DogName String
  deriving (Eq, Show)


newtype Address = Address String
  deriving (Eq, Show)


data Person = Person
  { humanName :: HumanName
  , dogName   :: DogName
  , address   :: Address
  } deriving (Eq, Show)


data Dog = Dog
  { dogsName    :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)


newtype Reader r a = Reader { runReader :: r -> a }


instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra


instance Applicative (Reader r) where
  pure a = Reader $ const a
  Reader rab <*> Reader ra = Reader $ \r -> rab r (ra r)


-- 1)
instance Monad (Reader r) where
  return = pure
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  Reader ra >>= aRb = Reader $ \r -> runReader (aRb $ ra r) r


-- 2)
getDogRM :: Reader Person Dog
getDogRM = Reader $ dogName >>= \d -> address >>= \a -> return $ Dog d a


me :: Person
me = Person (HumanName "Martin")
            (DogName "Snuggles")
            (Address "Bavaria")


main :: IO ()
main = print $ runReader getDogRM me

