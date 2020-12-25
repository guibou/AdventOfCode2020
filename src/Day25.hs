module Day25 where

import Data.List
import Utils

-- start: 11:12. 11:27

fileContent = (
  7573546,
  17786549)

-- * Generics
transform subjectNumber v = let
  v' = v * subjectNumber
  in v' `mod` 20201227

values = iterate (transform 7) 1

findLoopSize n = findIndex (==n) values

encryptionKey pk loopSize = applyN loopSize (transform pk) 1

-- * FIRST problem
ex0 = (5764801, 17807724)

day :: (Int, Int) -> Maybe Int
day (a, b) = encryptionKey a <$> findLoopSize b

-- * SECOND problem
day' :: _ -> Int
day' = undefined

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` Just 14897079
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` Just 7032853
