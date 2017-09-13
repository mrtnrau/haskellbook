module IdentityT where


newtype Identity a = Identity { runIdentity :: a }
  deriving (Show, Eq)


newtype IdentityT f a = IdentityT { runIdentityT :: f a }
  deriving (Show, Eq)


instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


instance Applicative Identity where
  pure = Identity
  Identity ab <*> Identity a = Identity (ab a)


instance Monad Identity where
  return = pure
  Identity a >>= f = f a


instance Functor f => Functor (IdentityT f) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)


instance Applicative f => Applicative (IdentityT f) where
  pure = IdentityT . pure
  IdentityT fab <*> IdentityT fa = IdentityT (fab <*> fa)


instance Monad m => Monad (IdentityT m) where
  return = pure
  IdentityT ma >>= aimb = IdentityT $ ma >>= runIdentityT . aimb
