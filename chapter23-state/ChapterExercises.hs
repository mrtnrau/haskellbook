module ChapterExercises where


import           Moi


-- 1)
get :: Moi s s
get = Moi $ \s -> (s, s)


-- 2)
put :: s -> Moi s ()
put s = Moi $ const ((), s)


-- 3)
exec :: Moi s a -> s -> s
exec (Moi sa) s = snd $ sa s


-- 4)
eval :: Moi s a -> s -> a
eval (Moi sa) s = fst $ sa s


-- 5)
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)


main :: IO ()
main = do
  print $ runMoi get "curryIsAmaze"
  print $ runMoi (put "blah") "woot"
  print $ exec (put "wilma") "daphne"
  print $ exec get "scooby papu"
  print $ eval get "bunnicula"
  print $ eval get "stake a bunny"
  print $ runMoi (modify (+1)) 0
  print $ runMoi (modify (+1) >> modify (+1)) 0



