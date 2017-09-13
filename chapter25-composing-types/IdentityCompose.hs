{-# LANGUAGE InstanceSigs #-}


module IdentityCompose where

-- Identity

newtype Identity a = Identity { runIdentity :: a }
  deriving (Show)


instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity a) = Identity (f a)


instance Applicative Identity where
  pure :: a -> Identity a
  pure = Identity

  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  Identity f <*> Identity a = Identity (f a)


-- Compose

newtype Compose f g a = Compose { getCompose :: f (g a) }
  deriving (Show)


instance ( Functor f
         , Functor g
         )
        => Functor (Compose f g) where
  fmap :: (a -> b) -> Compose f g a -> Compose f g b
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga


-- fgab  :: f (g (a -> b))
-- fga   :: f (g a)
-- (<*>) :: g (a -> b) -> g a -> g b
-- (<$>) :: (x -> y) -> f x -> f y
--
-- (<*>)                      <$> fgab
-- (x         -> y)           ->  f x            -> f y
-- g (a -> b) -> (g a -> g b) ->  f (g (a -> b)) -> f (g a -> g b)
--
-- ((<*>) <$> fgab) <*> fga
-- f (r   -> t)      -> f r     -> f t
-- f (g a -> g b)    -> f (g a) -> f (g b)
instance ( Applicative f
         , Applicative g
         )
        => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  Compose fgab <*> Compose fga = Compose $ (<*>) <$> fgab <*> fga


instance ( Foldable f
         , Foldable g
         )
        => Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose fga) = (foldMap . foldMap) f fga


instance ( Traversable f
         , Traversable g
         )
        => Traversable (Compose f g) where
  traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga


-- One

newtype One f a = One { getOne :: f a }
  deriving (Show)


instance Functor f => Functor (One f) where
  fmap :: (a -> b) -> One f a -> One f b
  fmap f (One fa) = One $ fmap f fa


instance Applicative f => Applicative (One f) where
  pure :: a -> One f a
  pure = One . pure

  (<*>) :: One f (a -> b) -> One f a -> One f b
  One fab <*> One fa = One $ fab <*> fa


-- Three

newtype Three f g h a = Three { getThree :: f (g (h a)) }
  deriving (Show)


instance ( Functor f
         , Functor g
         , Functor h
         )
        => Functor (Three f g h) where
  fmap :: (a -> b) -> Three f g h a -> Three f g h b
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha


-- fghab :: f (g (h (a -> b)))
-- fgha  :: f (g (h a))
--
-- (fmap . fmap) (<*>)                           fghab
--               (x          -> y)            -> f (g x)            -> f (g y)
--               (h (a -> b) -> (h a -> h b)) -> f (g (h (a -> b))) -> f (g (h a -> h b))
--
-- fghahb :: f (g (x -> y))
--   fgha :: f (g x)
--      x :: h a
--      y :: h b
-- (<*>)                      <$> fghahb
-- g (x -> y) -> (g x -> g y)  -> f (g x) -> f (g x -> g y)
--
-- ((<*>) <$> fghahb) <*> fgha
-- f (g x -> g y)      -> f (g x) -> f (g y) === f (g (h b))
--
-- holy moly
instance ( Applicative f
         , Applicative g
         , Applicative h
         )
        => Applicative (Three f g h) where
  pure :: a -> Three f g h a
  pure = Three . pure . pure . pure

  (<*>) :: Three f g h (a -> b) -> Three f g h a -> Three f g h b
  Three fghab <*> Three fgha =
    Three $ let fghahb = (fmap . fmap) (<*>) fghab
            in (<*>) <$> fghahb <*> fgha
