module SemVer where


import           Control.Applicative ((<|>))
import           Data.Monoid         ((<>))
import           Text.Trifecta


-- reordering because numbers should have lower precedence than strings
data NumberOrString =
    NOSI Integer
  | NOSS String
  deriving (Show, Eq, Ord)


type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]


data SemVer = SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)


instance Ord SemVer where
  compare (SemVer major1 minor1 patch1 release1 _)
          (SemVer major2 minor2 patch2 release2 _) =
    let x = compare major1 major2
         <> compare minor1 minor2
         <> compare patch1 patch2
    in case x of
         EQ -> case (release1, release2) of
                ([], []) -> EQ
                (l , []) -> LT
                ([], l ) -> GT
                _        -> compare release1 release2
         _  -> x


parseNoS :: Parser NumberOrString
parseNoS = (NOSS <$> some alphaNum) <|> (NOSI <$> natural)


parseRelease :: Parser Release
parseRelease = do
  char '-'
  sepBy1 parseNoS (char '.')


parseMetadata :: Parser Metadata
parseMetadata = do
  char '+'
  sepBy1 parseNoS (char '.')


-- major, minor and patch are non negative integers
-- but still allow leading zeros
-- e.g. parseString parseSemVer mempty "1.01.0"
--      should not be parsed succesful but is in the current implementation
-- but since the next exercise is parsing integers i drop this requirement
parseSemVer :: Parser SemVer
parseSemVer = do
  major <- natural
  char '.'
  minor <- natural
  char '.'
  patch <- natural
  release <- option [] parseRelease
  metadata <- option [] parseMetadata
  return $ SemVer major minor patch release metadata


main :: IO ()
main = do
  let p = parseString parseSemVer mempty
  print $ p "2.1.1"
  print $ p "1.0.0-alpha"
  print $ p "1.0.0-alpha.1"
  print $ p "1.0.0-0.3.7"
  print $ p "1.0.0-x.7.z.92"
  print $ p "1.0.0-alpha+001"
  print $ p "1.0.0+20130313144700"
  print $ p "1.0.0-beta+exp.sha.5114f85"

  print $ SemVer 1 0 0 [] [] < SemVer 2 0 0 [] []
  print $ SemVer 2 0 0 [] [] < SemVer 2 1 0 [] []
  print $ SemVer 2 1 0 [] [] < SemVer 2 1 1 [] []

  let a = SemVer 1 0 0 [NOSS "alpha"] []
      b = SemVer 1 0 0 [NOSS "alpha", NOSI 1] []
      c = SemVer 1 0 0 [NOSS "alpha", NOSS "beta"] []
      d = SemVer 1 0 0 [NOSS "beta"] []
      e = SemVer 1 0 0 [NOSS "beta", NOSI 2] []
      f = SemVer 1 0 0 [NOSS "beta", NOSI 11] []
      g = SemVer 1 0 0 [NOSS "rc", NOSI 1] []
      h = SemVer 1 0 0 [] []
  print $ a < b
  print $ b < c
  print $ c < d
  print $ d < e
  print $ e < f
  print $ f < g
  print $ g < h
