module Day02 where

import Utils
import Text.Megaparsec


-- start: 10:22
-- first star: 10:31
-- first star: 10:33

fileContent :: _
fileContent = parseContent $(getFile)

data Policy = Policy (Int, Int) Char
  deriving (Show)

aChar = choice (map single ['a'..'z'])

parsePolicy = do
  n <- parseNumber
  void "-"
  n' <- parseNumber
  c <- aChar

  pure $ Policy (n, n') c

parseLine = do
  policy <- parsePolicy

  void ": "
  cs <- Text.Megaparsec.some aChar

  pure (policy, cs)

parseContent :: Text -> _
parseContent = unsafeParse (parseLine `sepBy` "\n")

-- * Generics


-- * FIRST problem
isValid ((Policy (nMin, nMax) c), s) = countC >= nMin && countC <= nMax
  where countC = countItem c s


day = length . filter isValid

-- * SECOND problem
isValid' ((Policy (nMin, nMax) c), s) = (inPosA && not inPosB) || (not inPosA && inPosB)
  where
    inPosA = s `unsafeIndex` (nMin - 1) == c
    inPosB = s `unsafeIndex` (nMax - 1) == c

day' = length . filter isValid'

-- * Tests

test :: Spec
test = do
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 645
    it "on second star" $ do
      day' fileContent `shouldBe` 737
