{-# LANGUAGE InstanceSigs #-}


module EitherT where


import           Control.Monad             (liftM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }


instance Functor m => Functor (EitherT e m) where
  fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea


instance Applicative m => Applicative (EitherT e m) where
  pure :: a -> EitherT e m a
  pure = EitherT . pure . pure

  (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  EitherT mab <*> EitherT ma = EitherT $ (<*>) <$> mab <*> ma


instance Monad m => Monad (EitherT e m) where
  return :: a -> EitherT e m a
  return = pure

  (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  EitherT ma >>= f = EitherT $ ma >>= \v ->
    case v of
      Left e  -> (return . Left) e
      Right a -> (runEitherT . f) a


instance MonadTrans (EitherT e) where
  lift :: Monad m => m a -> EitherT e m a
  lift = EitherT . liftM Right


instance MonadIO m => MonadIO (EitherT e m) where
  liftIO :: IO a -> EitherT e m a
  liftIO = EitherT . liftM Right . liftIO


swapEither :: Either e a -> Either a e
swapEither (Left e)  = Right e
swapEither (Right a) = Left a


swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ swapEither <$> ma


eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT amc bmc (EitherT mab) = mab >>= \v ->
  case v of
    Left a  -> amc a
    Right b -> bmc b
