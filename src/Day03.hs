module Day03 where

import Utils
import qualified Data.Set as Set
import qualified Data.Text as Text


-- 09:41
-- 10:01 (WTF!)
-- 10:05

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> Set (Integer, Integer)
parseContent t = Set.fromList $ do
  (l, y) <- zip (lines t) [0..]
  (c, x) <- zip (Text.unpack l) [0..]

  guard $ c == '#'
  pure (x, y)


-- * Generics

inGrid gridXSize grid (posX, posY) = newPos `Set.member` grid
  where
    newPos = (posX `mod` (gridXSize + 1), posY)


-- * FIRST problem
day grid slope = length $ snd $ partitionEithers $ do
  let
    Just gridXSize = viaNonEmpty maximum1 (map fst (Set.toList grid))
    Just gridYSize = viaNonEmpty maximum1 (map snd (Set.toList grid))

  walkGridAndCountTree (gridXSize, gridYSize) grid (0, 0) slope

walkGridAndCountTree size@(sizeX, sizeY) grid (posX, posY) slope@(dx, dy)
  | posY == sizeY = []
  | otherwise =
  let
    nextX = posX + dx
    nextY = posY + dy

    newPos = (nextX, nextY)
    f
      | inGrid sizeX grid newPos = (Right newPos:)
      | otherwise = (Left newPos:)

  in
    f $ walkGridAndCountTree size grid newPos slope

ex0 = parseContent [fmt|\
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#|]


-- * SECOND problem
day' :: _ -> _
day' grid = product $ map (day grid) [
  (1, 1),
  (3, 1),
  (5, 1),
  (7, 1),
  (1, 2)
  ]

-- * Tests

test :: Spec
test = do
  describe "examples" $ do
    it "on first star" $ do
      day ex0 (3, 1) `shouldBe` 7
    it "on second star" $ do
      day' ex0 `shouldBe` 336
  describe "works" $ do
    it "on first star" $ do
      day fileContent (3, 1) `shouldBe` 153
    it "on second star" $ do
      day' fileContent `shouldBe` 2421944712
