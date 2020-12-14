module Day13 where

import Utils
import Text.Megaparsec

--start 12:21. First 12:30 13:28

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = unsafeParse $ (,) <$> parseNumber @Integer <*> (parseV `sepBy` ",")

parseV = choice [
  "x" $> Nothing,
  Just <$> parseNumber @Integer
  ]

-- * Generics


-- * FIRST problem
ex0 = parseContent [fmt|\
939
7,13,x,x,59,x,31,19|]


day :: (Integer, [Maybe Integer]) -> Maybe Integer
day (start, catMaybes -> buses) = do
  (a , b) <- viaNonEmpty minimum1 $ map (\x -> (x - (start `mod` x), x)) buses
  pure $ a * b

-- * SECOND problem
ex1 = parseContent [fmt|\
0
7,13,x,x,59,x,31,19
|]

day' :: [Maybe Integer] -> Integer
day' val = resteChinois decaList `mod` (product (map snd decaList))
  where
  decaList = map (\(a, b) -> ((b - a) `mod` b, b)) $ catMaybes $ zipWith (\a b -> (a,) <$> b) [0..] val

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` Just 295
    it "of second star" $ do
      day' (snd ex1) `shouldBe` 1068781
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` Just 246
    it "on second star" $ do
      day' (snd fileContent) `shouldBe` 939490236001473
