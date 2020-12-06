module Day06 where

import Utils
import qualified Data.Text as Text
import qualified Data.Set as Set

-- start: 13:27
-- 13:32
-- 13:35

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = map lines . Text.splitOn "\n\n"

-- * Generics
ex0 = parseContent [fmt|abc

a
b
c

ab
ac

a
a
a
a

b|]

foo f = sum . map (Set.size . f . map (Set.fromList . Text.unpack))

-- * FIRST problem
day :: _ -> Int
day = foo Set.unions

-- * SECOND problem
ex1 = parseContent [fmt|abc

a
b
c

ab
ac

a
a
a
a

b|]

itSets :: [Set Char] -> Set Char
itSets = foldl' Set.intersection (Set.fromList ['a'..'z'])

day' :: _ -> Int
day' = foo itSets

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 11
    it "of second star" $ do
      day' ex1 `shouldBe` 6
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 6735
    it "on second star" $ do
      day' fileContent `shouldBe` 3221
