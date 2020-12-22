module Day22 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Data.Set as Set

-- start: 17:45
-- first star: 17:52
-- second star: 18:08

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t = let
  (p0, Text.drop 2 -> p1) = Text.breakOn "\n\n" t

  parsePlayer (drop 1 . Text.lines -> l) = map (Unsafe.read @Int . Text.unpack) l

  in (parsePlayer p0, parsePlayer p1)

-- * Generics
score cards = sum $ zipWith (*) [1..] (reverse cards)

-- * FIRST problem
runGame _ ([], p) = (P0, p)
runGame _ (p, []) = (P1, p)
runGame f (x:xs, y:ys) = case compare x y of
  LT -> f (xs, ys ++ [y, x])
  GT -> f (xs ++ [x, y], ys)
  EQ -> error "Not specified in the exercise. That's sad, my kid loves this part of the card game"

ex0 = parseContent [fmt|\
Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10|]


day = score . snd . fix runGame

-- * SECOND problem
data Player = P0 | P1

runGameRecursive = go Set.empty
  where
    go :: Set ([Int], [Int]) -> ([Int], [Int]) -> (Player, [Int])
    go currentSet ps@(p0, p1)
      | ps `Set.member` currentSet = (P0, p0)
      | otherwise = case (p0, p1) of
          ([], p) -> (P1, p)
          (p, []) -> (P0, p)
          (x:xs, y:ys)
            | length xs >= x && length ys >= y -> case fst $ runGameRecursive (take x xs, take y ys) of
                P0 -> go nextSet (xs ++ [x, y], ys)
                P1 -> go nextSet (xs, ys ++ [y, x])
            | otherwise -> runGame (go nextSet) (p0, p1)
      where
        nextSet = Set.insert ps currentSet

day' = score . snd . runGameRecursive

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 306
    it "of second star" $ do
      day' ex0 `shouldBe` 291
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 32199
    it "on second star" $ do
      day' fileContent `shouldBe` 33780
