module Day01 where

import Utils
import qualified Data.Set as Set

-- start 17:45
-- firs star: 17:50
-- second start: 17:51

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [Int]
parseContent = map unsafeRead . lines

-- * Generics


-- * FIRST problem
day :: [Int] -> Maybe Int
day l = let
  entries = Set.fromList l

  f x = (2020 - x) `Set.member` entries

  in do
    x <- find f l
    pure (x * (2020 - x))

-- * SECOND problem
day' :: [Int] -> _
day' l = let
  entries = Set.fromList l
  in head $ do
    x <- l
    y <- l
    guard $ (2020 - x - y) `Set.member` entries
    pure (x * y * (2020 - x - y))


-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` Just 1019371
    it "on second star" $ do
      day' fileContent `shouldBe` Just 278064990
