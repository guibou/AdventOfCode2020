module Day10 where

import Utils
import qualified Relude.Unsafe as Unsafe
import Relude.Extra
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec
import Data.Function.Memoize

-- 15:56 -> 16:06 -> 16:21

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [Int]
parseContent t = (Unsafe.read . Text.unpack) <$> Text.lines t

-- * Generics
ex0 = parseContent [fmt|16
10
15
5
1
11
7
19
6
12
4|]

-- * FIRST problem
day :: [Int] -> Int
day (sort->l) = let
  (_, d1, d3) = foldl' f (0, 0, 1) l
  in d1 * d3
  where
    f (lastVoltage, count1, count3) newVoltage = (newVoltage,
                                                  count1 + bool 0 1 (dV == 1),
                                                  count3 + bool 0 1 (dV == 3))
      where
        dV = newVoltage - lastVoltage


-- * SECOND problem
day' :: _ -> Int
day' = arange

arange' f prev [x]
  | x - prev <= 3 = 1
  | otherwise = 0
arange' f prev (x:xs)
  | x - prev <= 3 = f x xs + f prev xs
  | otherwise = 0

arange :: [Int] -> Int
arange = (memoFix2 arange') 0 . sort

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 35
    it "of second star" $ do
      day' ex0 `shouldBe` 8
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2343
    it "on second star" $ do
      day' fileContent `shouldBe` 31581162962944
