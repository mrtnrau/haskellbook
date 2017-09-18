module OuterInner where


import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader


embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1


maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded


-- () -> IO (Either String (Maybe Int))
eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap


-- Wrap It Up
embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT $ ExceptT $ ReaderT $ const (return (Right (Just 1)))


test1 :: ReaderT () IO Int
test1 = ReaderT $ const (return 1)


test2 :: ExceptT String (ReaderT () IO) Int
test2 = ExceptT $ ReaderT $ const (return (Right 1))


test3 :: MaybeT (ExceptT String (ReaderT () IO)) Int
test3 = MaybeT $ ExceptT $ ReaderT $ const (return (Right (Just 1)))




