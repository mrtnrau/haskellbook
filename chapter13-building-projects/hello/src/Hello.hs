module Hello
    ( hello
    ) where

hello :: String -> IO ()
hello name = putStrLn $ "Hello " ++ name ++ "!"
