module Day20 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Data.Map as Map
import Linear (V2(..))

-- start:: 14:19. first star: 15:39.
-- 16:41 all is coded for second star, just it is wrong...
-- 16:55: done

-- There is room for optimisation
-- First: Use Int for `B` instead of String
-- Sec: instead of using combination of possibilities, use a Map

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t = let
  tiles = Text.splitOn "\n\n" t
  decode (Text.lines -> ~(header:rest)) = ((Unsafe.read @Int $ Text.unpack $ Text.take (Text.length n - 1) n), borders $ map Text.unpack rest)
    where
      n = Text.drop 5 header

  in map decode tiles

-- * Generics
mirrors b = [
  b,
  mirrorX b,
  mirrorY b
  ]

transform :: ((B, B, B, B), [B]) -> [((B, B, B, B) , [B])]
transform b = mirrors b <> mirrors (rotate90 b) <> mirrors (rotate90 (rotate90 b)) <> mirrors (rotate90 (rotate90 (rotate90 b)))

rotate90 ((t, b, l, r), shape) = ((reverse l, reverse r, b, t), map reverse (transpose shape))

mirrorX ((t, b, l, r), shape) = ((reverse t, reverse b, r, l), map reverse shape)
mirrorY ((t, b, l, r), shape) = ((b, t, reverse l, reverse r), transpose (map reverse (transpose shape)))

keys' m = (Unsafe.head m, Unsafe.last m)

borders :: [B] -> ((B, B, B, B), [B])
borders m = ((t, b, l ,r), m)
  where
     ((t,b), (l,r)) = (keys' m, keys' (transpose m))

type B = String

data ConstraintT = Constraint (Maybe String) (Maybe String) (Maybe String) (Maybe String)
  deriving (Show)

matchConstraint' _ Nothing = True
matchConstraint' v' (Just v) = v == v'

matchConstraint (a, b, c, d) (Constraint a' b' c' d')
  = matchConstraint' a a'
  && matchConstraint' b b'
  && matchConstraint' c c'
  && matchConstraint' d d'

mergeConstraint (Constraint a b c d) (Constraint a' b' c' d') = Constraint (merge a a') (merge b b') (merge c c') (merge d d')
  where
    merge Nothing Nothing = Nothing
    merge Nothing v = v
    merge v Nothing = v
    merge (Just v) (Just v')
      | v == v' = Just v
      | otherwise = error "Merge issue"

buildConstraint (t, b, l, r) (V2 x y) = Map.fromList $ [
      (V2 x (y-1), Constraint Nothing (Just t) Nothing Nothing),
      (V2 x (y+1), Constraint (Just b) Nothing Nothing Nothing),
      (V2 (x+1) y, Constraint Nothing Nothing (Just r) Nothing),
      (V2 (x-1) y, Constraint Nothing Nothing Nothing (Just l))
     ]
solve :: [(Int, ((B, B, B, B), [B]))] -> _
solve [] = error "Cannot solve an empty list"
solve ((firstId,((t, b, l, r), shape)):grids) = go grids firstConstraints (Map.singleton (V2 0 0) (firstId, shape))
  where
    firstConstraints = buildConstraint (t, b, l, r) (V2 0 0)

    go :: [(Int, ((B, B, B, B), [B]))] -> Map (V2 Int) ConstraintT -> Map (V2 Int) (Int, [B]) -> _
    go [] _ res = res
    go cases constraints res = let
      allCases :: [(Int, ((B, B, B, B), [B]))] = concatMap (\(i,c) -> (i,) <$> transform c) cases

      allConstraints :: [((V2 Int), ConstraintT)] = Map.toList constraints

      allCombinations = (,) <$> allCases <*> allConstraints

      oneToPlace = find (\((_gridId, (grid, _)), (_cpos, constraint)) -> matchConstraint grid constraint) allCombinations
      in case oneToPlace of
           Nothing -> error "WTF"
           Just ((tileId, (tileBorders, tileShape)), (pos, _)) -> let
             newConstraints = buildConstraint tileBorders pos

             in go (removeCase tileId cases) (Map.unionWith mergeConstraint constraints newConstraints) (Map.insert pos (tileId, tileShape) res)

removeCase :: Int -> [(Int, _)] -> [(Int, _)]
removeCase i = filter (\(i', _) -> i /= i')

-- * Part 2

buildImage :: Map (V2 Int) [B] -> Text
buildImage m = let
  tiles = Map.map removeBorders m

  tileSizes = let
    firstTile = Unsafe.fromJust (Map.lookup (V2 0 0) tiles)
    in length firstTile

  toIdx :: ((V2 Int), [B]) -> [((V2 Int), Text)]
  toIdx (V2 bigX bigY, content) = do
    (dy, l) <- zip [0..] content
    (dx, c) <- zip [0..] l

    pure (V2 (bigX * tileSizes + dx) (bigY * tileSizes + dy), Text.singleton c)

  bigMap = Map.fromList $ concatMap toIdx (Map.toList tiles)

  in str2DGrid (Map.map toText bigMap)

removeBorders mat = let sub = transpose (take (length mat - 2) (drop 1 mat))
                   in transpose (take (length sub -2) (drop 1 sub))

solution content = let
  simpleImage = buildImage $ (Map.map snd $ solve content)

  allImages = map snd $ transform ((undefined, undefined, undefined, undefined), map (Text.unpack) $ Text.lines simpleImage)
  in do
  res <- find (not . null) $ map locateSeaMonster allImages

  let
    countSharp im = length (filter (== '#') im)
    countSharp' im = sum (map countSharp im)

  pure $ countSharp (Text.unpack simpleImage) - length res * countSharp' seaMonster



seaMonster = [
  "                  # ",
  "#    ##    ##    ###",
  " #  #  #  #  #  #   "
  ]

locateSeaMonster image = do
  let
    imageHeight = length image
    imageWidth = length (Unsafe.head image)

    seaMonsterHeight = length seaMonster
    seaMonsterWidth = length (Unsafe.head seaMonster)

  x <- [0..imageWidth-1]
  y <- [0..imageHeight-1]

  guard $ isSeaMonster (crop (x, y) (seaMonsterWidth, seaMonsterHeight) image)

  pure (x, y)

crop (dx, dy) (w, h) im = map (take w . drop dx) $ take h (drop dy im)

isSeaMonster :: [String] -> Bool
isSeaMonster image = all (uncurry matchLine) (zip (image ++ repeat []) seaMonster)

matchLine :: String -> String -> Bool
matchLine _ [] = True
matchLine [] _ = False
matchLine (im:ims) (sea:seas)
  | sea == '#' = im == '#' && matchLine ims seas
  | otherwise = matchLine ims seas


-- * FIRST problem
ex0 = parseContent [fmt|\
Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...|]

day input = let
  res = Map.map fst $ solve input
  (V2 minX minY, V2 maxX maxY) = getBounds (Map.keys res)

  in product $ map (\c -> Unsafe.fromJust $ Map.lookup c res) $ [
  V2 minX minY,
  V2 minX maxY,
  V2 maxX minY,
  V2 maxX maxY]

-- * SECOND problem
day' = solution


-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 20899048083289
    it "of second star" $ do
      day' ex0 `shouldBe` Just 273
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 68781323018729
    it "on second star" $ do
      day' fileContent `shouldBe` Just 1629
