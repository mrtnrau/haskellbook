{-# LANGUAGE InstanceSigs #-}


module StateT where


import           Control.Monad             (liftM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class


newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }


instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT sma) = StateT $ \s -> g <$> sma s
    where g (a, s) = (f a, s)


instance Monad m => Applicative (StateT s m) where
  pure :: a -> StateT s m a
  pure a = StateT $ \s -> pure (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT smab <*> StateT sma = StateT $
    \s -> do
      (f, s')  <- smab s
      (a, s'') <- sma s'
      return (f a, s'')


instance Monad m => Monad (StateT s m) where
  return :: a -> StateT s m a
  return = pure

  (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sma >>= f = StateT $
    \s -> do
      (a, s') <- sma s
      runStateT (f a) s'


instance MonadTrans (StateT s) where
  lift :: (Monad m) => m a -> StateT s m a
  lift ma = StateT $ \s -> liftM (flip (,) s) ma


instance MonadIO m => MonadIO (StateT s m) where
  liftIO :: IO a -> StateT s m a
  liftIO ioa = StateT $ \s -> (liftM (flip (,) s) . liftIO) ioa


