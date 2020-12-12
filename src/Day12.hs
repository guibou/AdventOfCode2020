module Day12 where

import Utils
import Text.Megaparsec

-- start 15:04 -> 15:26. 15:37

data Direction = N | E | S | W
  deriving (Show, Eq, Bounded, Enum)

data Motion = MotionDirection Direction | L | R | F
  deriving Show

fileContent :: _
fileContent = parseContent $(getFile)

parseMotion = choice [
  MotionDirection <$> choice [
  "N" $> N,
  "S" $> S,
  "E" $> E,
  "W" $> W
  ],
  "L" $> L,
  "R" $> R,
  "F" $> F
  ]

data Action = Action Motion Int
  deriving (Show)

parseAction = Action <$> parseMotion <*> parseNumber

parseContent :: Text -> _
parseContent = unsafeParse (Text.Megaparsec.some parseAction)

-- * Generics
ex0 = parseContent [fmt|\
F10
N3
F7
R90
F11|]

-- * FIRST problem
data Ship = Ship Direction (Int, Int)
  deriving (Show)

startShip = Ship E (0, 0)

stepShip :: Action -> Ship -> Ship
stepShip (Action m i) ship@(Ship currentDir _) = case m of
  MotionDirection d -> move d i ship
  L -> turn i ship
  R -> turn (360 - i) ship
  F -> move currentDir i ship

move d i (Ship currentDir (px, py)) = Ship currentDir (px + dx, py + dy)
  where
    (dx, dy) = case d of
      N -> (0, -i)
      S -> (0, i)
      E -> (i, 0)
      W -> (-i, 0)

turn 0 ship = ship
turn deg (Ship curDir pos) = turn (deg - 90) (Ship (prev curDir) pos)

stepsShip actions = foldl' (flip stepShip) startShip actions

day actions = let
  Ship _ (x, y) = stepsShip actions
  in abs x + abs y

-- * SECOND problem
data ShipWP = ShipWP (Int, Int) (Int, Int)
  deriving (Show)

startShipWP = ShipWP (10, -1) (0, 0)

stepShipWP :: Action -> ShipWP -> ShipWP
stepShipWP (Action m i) ship@(ShipWP currentWP _) = case m of
  MotionDirection d -> moveWP d i ship
  L -> turnWP i ship
  R -> turnWP (360 - i) ship
  F -> moveShipWP currentWP i ship

moveShipWP (wpX, wpY) i (ShipWP currentWP (x, y)) = ShipWP currentWP (x + i * wpX, y + i * wpY)

moveWP d i (ShipWP (wx, wy) pos) = ShipWP (wx + dx, wy + dy) pos
  where
    (dx, dy) = case d of
      N -> (0, -i)
      S -> (0, i)
      E -> (i, 0)
      W -> (-i, 0)

turnWP 0 ship = ship
turnWP deg (ShipWP curWP pos) = turnWP (deg - 90) (ShipWP (nextWP curWP) pos)

nextWP (x, y) = (y, -x)

stepsShipWP actions = foldl' (flip stepShipWP) startShipWP actions

day' actions = let
  ShipWP _ (x, y) = stepsShipWP actions
  in abs x + abs y

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 25
    it "of second star" $ do
      day' ex0 `shouldBe` 286
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 636
    it "on second star" $ do
      day' fileContent `shouldBe` 26841
