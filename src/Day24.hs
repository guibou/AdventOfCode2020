module Day24 (test) where

import Utils
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Text.Megaparsec
import Linear (V2(..))

-- start: 10:32
-- first star: 10:46
-- second star: 10:55

fileContent :: _
fileContent = parseContent $(getFile)

parseDirection = choice [
  "e" $> E,
  "se" $> SE,
  "sw" $> SW,
  "w" $> W,
  "nw" $> NW,
  "ne" $> NE
  ]

parsePath = Text.Megaparsec.some parseDirection
parseContent = unsafeParse $ parsePath `sepBy` "\n"


-- * Generics
data Direction = E | SE | SW | W | NW | NE
  deriving (Show, Enum, Bounded)

move d (V2 x y) = case d of
  E -> V2 (x + 2) y
  W -> V2 (x - 2) y
  SW -> V2 (x - 1) (y - 1)
  SE -> V2 (x + 1) (y - 1)
  NE -> V2 (x + 1) (y + 1)
  NW -> V2 (x - 1) (y + 1)

runPath :: [Direction] -> V2 Int
runPath l = foldl' (flip move) (V2 0 0) l

-- * FIRST problem
ex0 = parseContent [fmt|\
sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew|]

colorize paths = HashSet.toList $ foldl' f HashSet.empty (map runPath paths)
  where
    f :: HashSet (V2 Int) -> V2 Int -> HashSet (V2 Int)
    f s p
      | p `HashSet.member` s = HashSet.delete p s
      | otherwise = HashSet.insert p s

day = length . colorize

-- * SECOND problem
adj x = do
  d <- universe
  pure $ move d x


stepLobby currentBlack = do
  let blackSet = HashSet.fromList currentBlack
  (point, neightboor) <- HashMap.toList $ (HashMap.unionWith ((+) @Int) (HashMap.fromListWith (+) . map (,1) $ concatMap adj currentBlack) (HashMap.fromList (map (,0) currentBlack)))

  let
    isBlack = point `HashSet.member` blackSet

  guard $ (not isBlack && neightboor == 2) || (isBlack && not (neightboor == 0 || neightboor > 2))

  pure point

day' :: _ -> Int
day' = length . applyN 100 stepLobby . colorize

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 10
    it "of second star" $ do
      day' ex0 `shouldBe` 2208
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 332
    it "on second star" $ do
      day' fileContent `shouldBe` 3900
