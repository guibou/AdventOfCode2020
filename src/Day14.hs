module Day14 where

import Utils
import qualified Data.Map as Map
import Text.Megaparsec

-- start 09:30. 09:45 -> 10:00

fileContent :: _
fileContent = parseContent $(getFile)

data MaskAtom = X | O | I
  deriving (Show)

data Action = Mem Int Int | Mask [MaskAtom]
  deriving (Show)

parseMem = do
  void "mem["
  offset <- parseNumber
  void "] = "
  n <- parseNumber
  pure $ Mem offset n

parseMask = do
  void "mask = "
  mask <- lexeme (count 36 parseAtom)
  pure $ Mask mask

parseAtom = choice ["X" $> X,
                    "1" $> I,
                    "0" $> O
                   ]

parseLine = parseMem <|> parseMask


parseContent :: Text -> _
parseContent = unsafeParse (Text.Megaparsec.some parseLine)

-- * Generics
overrideWithMask value mask = go value (reverse mask)
  where
    go _ [] = 0
    go val (x:xs) = 2 * go (val `div` 2) xs + case x of
      O -> 0
      I -> 1
      X -> val `mod` 2

runMachine :: [Action] -> Map Int Int
runMachine = snd . (foldl' f (undefined, Map.empty))
  where
    f (_currentMask, m) (Mask t) = (t, m)
    f (currentMask, m) (Mem offset n) = (currentMask, Map.insert offset (overrideWithMask n currentMask) m)

-- * FIRST problem
ex0 = parseContent [fmt|\
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0|]

day :: _ -> Int
day = sum . Map.elems . runMachine

-- * SECOND problem
floatingMask value mask = go value (reverse mask)
  where
    go :: Int -> [MaskAtom] -> [Int]
    go _ [] = [0]
    go val (x:xs) = let
      sub = (2*) <$> go (val `div` 2) xs
      in case x of
        O -> (+ (val `mod` 2)) <$> sub
        I -> (+1) <$> sub
        X -> sub ++ ((+1) <$> sub)

runMachine' :: [Action] -> _
runMachine' = snd . (foldl' f (error "using an unset mask", Map.empty))
  where
    f (_currentMask, m) (Mask t) = (t, m)
    f (currentMask, mem) (Mem initialOffset n) = (currentMask, foldl' write mem (floatingMask initialOffset currentMask))
      where
        write m' offset = Map.insert offset n m'

ex1 = parseContent [fmt|mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1|]

day' :: _ -> Int
day' = sum . Map.elems . runMachine'

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 165
    it "of second star" $ do
      day' ex1 `shouldBe` 208
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 12408060320841
    it "on second star" $ do
      day' fileContent `shouldBe` 4466434626828
