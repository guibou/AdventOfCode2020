module Day05 where

import Utils
import qualified Data.Text as Text

-- date: 13:57
-- first 14:03
-- second: 16:06

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = lines

-- * Generics
decode :: Text -> (Int, Int)
decode t =
  let
    row = Text.take 7 t
    col = Text.drop 7 t

  in (fbToInt row, lrToInt col)

toInt :: Char -> Char -> Text -> Int
toInt z o v = foldl' f 0 (Text.unpack v)
  where
    f i x
      | x == z = i * 2
      | x == o = i * 2 + 1
      | otherwise = error [fmt|Unexpected value {[x]} in {v}|]

fbToInt = toInt 'F' 'B'
lrToInt = toInt 'L' 'R'

seatId :: (Int, Int) -> Int
seatId (row, col) = row * 8 + col

-- * FIRST problem
day :: [Text] -> Int
day = maximum . map (seatId . decode)

-- * SECOND problem
day' :: _ -> Maybe Int
day' content = let
  ids = sort $ map (seatId . decode) content
  in (+1) . fst <$> find (\(x, y) -> (x+1) /= y) (zip ids (unsafeTail ids))

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      decode "BFFFBBFRRR" `shouldBe` (70, 7)
      decode "FFFBBBFRRR" `shouldBe` (14, 7)
      decode "BBFFBBFRLL" `shouldBe` (102, 4)
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 888
    it "on second star" $ do
      day' fileContent `shouldBe` (Just 522)
