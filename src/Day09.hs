module Day09 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Data.HashSet as HashSet
import qualified Data.Vector as Vector

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
  nums = Vector.fromList numbers

  go !offsetStart !offsetEnd !currentSum = case compare currentSum v of
    EQ -> let
      slice = Vector.slice offsetStart (offsetEnd - offsetStart + 1) nums
      in Vector.minimum slice + Vector.maximum slice
    LT -> go offsetStart (offsetEnd + 1) (currentSum + nums Vector.! (offsetEnd + 1))
    GT -> go (offsetStart + 1) offsetEnd (currentSum - nums Vector.! offsetStart)
  in go 0 1 (nums Vector.! 0 + nums Vector.! 1)

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day 5 ex0 `shouldBe` 127
    it "of second star" $ do
      day' 5 ex0 `shouldBe` 62
  describe "works" $ do
    it "on first star" $ do
      day 25 fileContent `shouldBe` 85848519
    it "on second star" $ do
      day' 25 fileContent `shouldBe` 13414198
