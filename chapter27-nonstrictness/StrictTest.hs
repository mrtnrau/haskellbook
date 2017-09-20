{-# LANGUAGE Strict #-}


module StrictTest where


willForce x = 1


willNotForce ~x = 1


main = do
  print (willNotForce undefined)
  print (willForce undefined)
