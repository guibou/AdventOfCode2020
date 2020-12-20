{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Day17 where

import Utils
import qualified Data.Text as Text
import qualified Data.Set as Set
import Linear

-- start: 19:04
-- first star: 19:19
-- second star: 20:11

fileContent :: ExtendedFlatSpace t a => Set t
fileContent = parseContent [fmt|\
....###.
#...####
##.#.###
..#.#...
##.#.#.#
#.######
..#..#.#
######.#|]


class (Num t, Ord t, Num b, Enum b, Eq b) => ExtendedFlatSpace t b | t -> b where
  fromFlat :: b -> b -> t
  delta :: [t]

instance (Ord t, Num t, Enum t) => ExtendedFlatSpace (V3 t) t where
  fromFlat x y = V3 x y 0

  delta = do
    dx <- [-1..1]
    dy <- [-1..1]
    dz <- [-1..1]

    let d = V3 dx dy dz

    guard $ d /= V3 0 0 0
    pure $ d

instance (Ord t, Num t, Enum t) => ExtendedFlatSpace (V4 t) t where
  fromFlat x y = V4 x y 0 0

  delta = do
    dx <- [-1..1]
    dy <- [-1..1]
    dz <- [-1..1]
    w <- [-1..1]

    let d = V4 dx dy dz w

    guard $ d /= V4 0 0 0 0

    pure d

parseContent :: ExtendedFlatSpace t a => Text -> Set t
parseContent t = Set.fromList $ do
  (l, line) <- zip [0..] (Text.lines t)
  (c, item) <- zip [0..] (Text.unpack line)

  guard $ item == '#'

  pure $ fromFlat c l

-- * Generics
copains p = do
  d <- delta
  pure $ p + d

step :: (Show t, Num t) => ExtendedFlatSpace t a => Set t -> Set t
step actives = Set.fromList $ do
  let interesting = ordNub (Set.toList actives >>= copains)
  candidate <- interesting

  let
    active = Set.member candidate actives
    activeNeighbors = length (filter (\c -> Set.member c actives) (copains candidate))

  guard $ (active && (activeNeighbors == 2 || activeNeighbors == 3)) || (not active && activeNeighbors == 3)

  pure candidate

-- * FIRST problem
ex0 :: ExtendedFlatSpace t a => Set t
ex0 = parseContent [fmt|\
.#.
..#
###|]

applyN f n = foldl' (.) id (replicate n f)

day :: (Show t, ExtendedFlatSpace t a) => Set t -> Int
day = length . applyN step 6

-- * SECOND problem
day' :: _ -> Int
day' = undefined

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day @(V3 Int) ex0 `shouldBe` 112
    it "of second star" $ do
      day @(V4 Int) ex0 `shouldBe` 848
  describe "works" $ do
    it "on first star" $ do
      day @(V3 Int) fileContent `shouldBe` 333
    it "on second star" $ do
      day @(V4 Int) fileContent `shouldBe` 2676
