module IPvX where


import           Data.Bits
import           Data.Char     (digitToInt)
import           Data.Word
import           Text.Printf
import           Text.Trifecta


newtype IPv4Address = IPv4Address Word32
  deriving (Eq, Ord)


instance Show IPv4Address where
  show (IPv4Address word32) = printf "%d.%d.%d.%d" a b c d
    where [a, b, c, d] = shiftIP [24, 16, 8, 0] 255 word32


data IPv6Address = IPv6Address Word64 Word64
  deriving (Eq, Ord)


instance Show IPv6Address where
  show (IPv6Address higher lower) =
    printf "%x:%x:%x:%x:%x:%x:%x:%x" a b c d e f g h
    where offsets = [48, 32, 16, 0]
          [a, b, c, d] = shiftIP offsets 65535 higher
          [e, f, g, h] = shiftIP offsets 65535 lower


shiftIP :: (Num a, Bits a) => [Int] -> a -> a -> [a]
shiftIP offsets mask num =
  (\offset -> shiftR num offset .&. mask) <$> offsets


parseIPv4 :: Parser IPv4Address
parseIPv4 = do
  octets <- parseIPv4Octet `sepBy` char '.'
  if length octets /= 4 || any (>255) octets
  then fail "invalid ipv4 address"
  else return $ IPv4Address (shiftCombine [24,16,8,0] octets)


parseIPv4Octet :: Parser Word32
parseIPv4Octet = toNumFromBase 10 <$> some digit


parseIPv6 :: Parser IPv6Address
parseIPv6 = do
  hextets <- parseIpv6Hextet `sepBy` char ':'
  let padded  = padWithZeros hextets
      offsets = [48,32,16,0]
      higher  = shiftCombine offsets (take 4 padded)
      lower   = shiftCombine offsets (drop 4 padded)
  return $ IPv6Address higher lower


parseIpv6Hextet :: Parser Word64
parseIpv6Hextet = toNumFromBase 16 <$> option "0" (some hexDigit)


padWithZeros :: (Eq a, Num a) => [a] -> [a]
padWithZeros nums = go (8 - length nums) nums
  where go 0   ns     = ns
        go len []     = 0 : go (len-1) []
        go len (n:ns) = if n == 0
                        then 0 : go (len-1) (n:ns)
                        else n : go len ns


shiftCombine :: (Num a, Bits a) => [Int] -> [a] -> a
shiftCombine offsets xs = foldl f 0 (zip offsets xs)
  where f b (offset, num) = b + shiftL num offset


toNumFromBase :: Num a => Int -> String -> a
toNumFromBase b = fromIntegral . fst . go
  where go ""     = (0, 1)
        go (x:xs) = let (acc, base) = go xs
                    in  (acc + digitToInt x * base, base * b)


ipv4Toipv6 :: IPv4Address -> IPv6Address
ipv4Toipv6 (IPv4Address word32) =
  IPv6Address 0 (shiftL 65535 32 + fromIntegral word32)


main :: IO ()
main = do
  let p i = parseString i mempty
      ipv41 = p parseIPv4 "172.16.254.1"
      ipv42 = p parseIPv4 "204.120.0.15"
      ipv61 = p parseIPv6 "FE80::0202:B3FF:FE1E:8329"
      ipv62 = p parseIPv6 "2001:DB8::8:800:200C:417A"
  print ipv41
  print ipv42
  print $ ipv4Toipv6 <$> ipv41
  print $ ipv4Toipv6 <$> ipv42
  print ipv61
  print ipv62
