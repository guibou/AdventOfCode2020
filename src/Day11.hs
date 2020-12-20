module Day11 where

import Utils
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Relude.Unsafe as Unsafe
import Linear

-- start: 20:25. With a pause for the kid, 21:02.
-- start computation at 21:17.

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> [V2 Int]
parseContent t = do
  (lc, l) <- zip [0..] (Text.lines t)
  (cc, c) <- zip [0..] (Text.unpack l)

  guard $ c == 'L'

  pure $ V2 cc lc

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


adj :: V2 Int -> [V2 Int]
adj (V2 x y) = do
  dx <- [-1..1]
  dy <- [-1..1]

  guard $ (dx, dy) /= (0, 0)

  pure $ V2 (x + dx) (y + dy)


-- * FIRST problem
day :: [V2 Int] -> Int
day seatsPos = length $ fixpoint (iterSeat seatsPos) Set.empty

iterSeat :: [V2 Int] -> Set (V2 Int) -> Set (V2 Int)
iterSeat seatsPos usedOnes = let
  in Set.fromList $ do
  seatPos <- seatsPos

  let occupiedNext = length (filter (\testPos -> testPos `Set.member` usedOnes) (adj seatPos))
      occupied = seatPos `Set.member` usedOnes

  guard $ (not occupied && occupiedNext == 0) || ((occupied && occupiedNext < 4))

  pure seatPos

-- * SECOND problem
adjDir :: [V2 Int]
adjDir = do
  dx <- [-1..1]
  dy <- [-1..1]

  guard $ (dx, dy) /= (0, 0)

  pure $ V2 dx dy

compute_grid_voisins seats = Map.fromList $ do
  let
    (_, V2 bx by) = getBounds seats
    seatSet = Set.fromList seats
    lookupInDir (V2 x y) (V2 dx dy)
      | x < 0 || x > bx || y < 0 || y > by = Nothing
      | newPos `Set.member` seatSet = Just newPos
      | otherwise = lookupInDir (V2 (x + dx) (y + dy)) (V2 dx dy)
      where newPos = V2 (x + dx) (y + dy)

  seat <- seats

  let copains = do
        dirCopain <- adjDir

        case lookupInDir seat dirCopain of
          Nothing -> mzero
          Just copain -> pure copain

  pure (seat, copains)



-- * FIRST problem
day' :: [V2 Int] -> Int
day' seatsPos = length $ fixpoint (iterSeat' seatsPos) Set.empty

iterSeat' :: [V2 Int] -> Set (V2 Int) -> Set (V2 Int)
iterSeat' seatsPos = let
  gridVoisins = compute_grid_voisins seatsPos
  in \usedOnes -> Set.fromList $ do
    seatPos <- seatsPos

    let occupiedNext = length $ filter (\p -> p `Set.member` usedOnes) (Unsafe.fromJust $ Map.lookup seatPos gridVoisins)
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
