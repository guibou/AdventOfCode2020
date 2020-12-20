module Day09 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Data.HashSet as HashSet

-- start: 19:00 --> 19:07 -> 19:33 (I'm stupid, my code was using the example value... Last 20 minutes because of that)

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [Integer]
parseContent = map (Unsafe.read . Text.unpack) . Text.lines

-- * Generics
day nP numbers = let
  preamble = take nP numbers
  n' = drop nP numbers

  check preamble (x:xs) = let
    pSet = HashSet.fromList preamble
    in if any (\j -> (x - j) `HashSet.member` pSet) preamble
       then check (drop 1 preamble ++ [x]) xs
       else x

  in check preamble n'



-- * FIRST problem
ex0 = parseContent [fmt|35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576|]

-- * SECOND problem
day' nP numbers = let
  v = day nP numbers
  in
  viaNonEmpty head $ do
    dd <- tails numbers
    ii <- inits dd

    guard $ length ii >= 2
    guard $ sum ii == v
    pure $ do
      a <- viaNonEmpty maximum1 ii
      b <- viaNonEmpty minimum1 ii

      pure $ a + b

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day 5 ex0 `shouldBe` 127
    it "of second star" $ do
      day' 5 ex0 `shouldBe` Just (Just 62)
  describe "works" $ do
    it "on first star" $ do
      day 25 fileContent `shouldBe` 85848519
    it "on second star" $ do
      day' 25 fileContent `shouldBe` Just (Just 13414198)
