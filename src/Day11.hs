module Day11 where

import Utils
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

-- start: 20:25. With a pause for the kid, 21:02.
-- start computation at 21:17.

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [(Int, Int)]
parseContent t = do
  (lc, l) <- zip [0..] (Text.lines t)
  (cc, c) <- zip [0..] (Text.unpack l)

  guard $ c == 'L'

  pure (cc, lc)

-- * Generics
ex0 = parseContent [fmt|\
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL|]


adj :: (Int, Int) -> [(Int, Int)]
adj (x, y) = do
  dx <- [-1..1]
  dy <- [-1..1]

  guard $ (dx, dy) /= (0, 0)

  pure (x + dx, y + dy)


-- * FIRST problem
day :: [(Int, Int)] -> Int
day seatsPos = length $ fixpoint (iterSeat seatsPos) Set.empty

iterSeat :: [(Int, Int)] -> Set (Int, Int) -> Set (Int, Int)
iterSeat seatsPos usedOnes = let
  in Set.fromList $ do
  seatPos <- seatsPos

  let occupiedNext = length (filter (\testPos -> testPos `Set.member` usedOnes) (adj seatPos))
      occupied = seatPos `Set.member` usedOnes

  guard $ (not occupied && occupiedNext == 0) || ((occupied && occupiedNext < 4))

  pure seatPos

setToMap :: [(Int, Int)] -> Set (Int, Int) -> _
setToMap s used = display2DGrid $ Map.fromList (map (\p -> (p, bool "L" "#" (p `Set.member` used))) s)


-- * SECOND problem
adjDir :: [(Int, Int)]
adjDir = do
  dx <- [-1..1]
  dy <- [-1..1]

  guard $ (dx, dy) /= (0, 0)

  pure (dx, dy)

adj' :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> (Int, Int) -> Int
adj' (bx, by) seats usedPos pos =
  let
    lookupInDir (x, y) (dx, dy)
      | x < 0 || x > bx || y < 0 || y > by = False
      | newPos `Set.member` seats = newPos `Set.member` usedPos
      | otherwise = lookupInDir (x + dx, y + dy) (dx, dy)
      where newPos = (x + dx, y + dy)
  in
    sum $ do
       d <- adjDir

       let used = lookupInDir pos d

       guard used

       pure 1


-- * FIRST problem
day' :: [(Int, Int)] -> Int
day' seatsPos = length $ fixpoint (iterSeat' seatsPos) Set.empty

iterSeat' :: [(Int, Int)] -> Set (Int, Int) -> Set (Int, Int)
iterSeat' seatsPos = let
  (_, bounds) = getBounds seatsPos
  lseats = Set.fromList seatsPos
  in \usedOnes -> Set.fromList $ do
    seatPos <- seatsPos

    let occupiedNext = adj' bounds lseats usedOnes seatPos
        occupied = seatPos `Set.member` usedOnes

    guard $ (not occupied && occupiedNext == 0) || ((occupied && occupiedNext < 5))

    pure seatPos

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 37
    it "of second star" $ do
      day' ex0 `shouldBe` 26
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2494
    it "on second star" $ do
      day' fileContent `shouldBe` 2306
