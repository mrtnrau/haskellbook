module ChapterExercises where


import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Functor.Identity


-- 1)
rDec :: Num a => Reader a a
rDec = ReaderT $ \a -> Identity (a - 1)


-- 2)
rDec' :: Num a => Reader a a
rDec' = ReaderT $ Identity . flip (-) 1


-- 3)
rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \a -> Identity (show a)


-- 4)
rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ Identity . show


-- 5)
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $
  \a -> do
    print $ "Hi: " ++ show a
    return (a + 1)


-- 6)
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $
  \a -> do
    let sa = show a
    print $ "Hi : " ++ sa
    return (sa, a + 1)



