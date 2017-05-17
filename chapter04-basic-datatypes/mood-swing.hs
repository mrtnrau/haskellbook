-- mood-swing.hs

module MoodSwing where

data Mood = Blah | Woot deriving Show

-- 1.) type constructor: Mood
-- 2.) data constructor/possible values: Blah, Woot

-- 3.) changeMood :: Mood -> Mood would be correct
--     no Data constructors on type level!

-- 4.)
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah
