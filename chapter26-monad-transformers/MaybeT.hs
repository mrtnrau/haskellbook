{-# LANGUAGE InstanceSigs #-}


module MaybeT where


import           Control.Monad             (liftM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class


newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }


instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma


instance Applicative m => Applicative (MaybeT m) where
  pure :: a -> MaybeT m a
  pure = MaybeT . pure . pure

  (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  MaybeT mab <*> MaybeT ma = MaybeT $ (<*>) <$> mab <*> ma


instance Monad m => Monad (MaybeT m) where
  return :: a -> MaybeT m a
  return = pure

  (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  MaybeT ma >>= f = MaybeT $ ma >>= \v ->
    case v of
      Nothing -> return Nothing
      Just a  -> (runMaybeT . f) a


instance MonadTrans MaybeT where
  lift :: Monad m => m a -> MaybeT m a
  lift = MaybeT . liftM Just


instance MonadIO m => MonadIO (MaybeT m) where
  liftIO :: IO a -> MaybeT m a
  liftIO = MaybeT . liftIO . liftM Just

