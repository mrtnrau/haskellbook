module LearnParsers where


import           Text.Trifecta


stop :: Parser a
stop = unexpected "stop"


one :: Parser Char
one = char '1'


one' :: Parser b
one' = one >> stop


oneTwo :: Parser Char
oneTwo = char '1' >> char '2'


oneTwo' :: Parser b
oneTwo' = oneTwo >> stop


testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"


pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)


-- Parsing Exercise
-- 1)
one'' :: Parser ()
one'' = one >> eof


oneTwo'' :: Parser ()
oneTwo'' = oneTwo >> eof


testEOF :: Parser () -> IO ()
testEOF p = print $ parseString p mempty "123"


-- 2)
testString :: Parser String -> IO ()
testString p = print $ parseString p mempty "123"


-- 3)
string' :: CharParsing m => String -> m String
string' s = sequenceA $ char <$> s


main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  -- Parsing Exercise
  -- 1)
  pNL "one'':"
  testEOF one''
  pNL "oneTwo'':"
  testEOF oneTwo''

  -- 2)
  pNL "string \"1\":"
  testString $ string "1"
  pNL "string \"12\":"
  testString $ string "12"
  pNL "string \"123\":"
  testString $ string "123"

  pNL "string \"1\"stop:"
  testString $ string "1" >> stop
  pNL "string \"12stop\":"
  testString $ string "12" >> stop
  pNL "string \"123stop\":"
  testString $ string "123" >> stop

  -- 3)
  pNL "string' \"1\":"
  testString $ string' "1"
  pNL "string' \"12\":"
  testString $ string' "12"
  pNL "string' \"123\":"
  testString $ string' "123"




