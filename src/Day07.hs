module Day07 where

import Utils
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec
import qualified Data.Text as Text
import qualified Data.Map as Map

-- start 18:22
-- first star: 18:49
-- second star: 18:52

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = Map.fromList . unsafeParse (parseBag `sepBy` "\n")

parseColor = do
  w <- parseWord
  void " "
  w' <- parseWord
  pure (w <> " " <> w')

parseWord :: Parser Text
parseWord = Text.pack <$> Prelude.many (oneOf ['a'..'z'])

parseBag :: Parser (Text, [(Int, Text)])
parseBag = do
  c <- parseColor
  void " bags contain "
  bags <- choice [
    ([] <$ "no other bags"),
    (parseCountedBag `sepBy` ", ")
    ]
  void "."

  pure (c, bags)

parseCountedBag :: Parser (Int, Text)
parseCountedBag = do
  i <- parseNumber
  c <- parseColor
  void " bag"
  void $ optional "s"

  pure (i, c)

-- * Generics
bagSize :: Text -> Map Text [(Int, Text)] -> Int
bagSize name m = let
  bagSizeMap = Map.map (\l -> 1 + sum (map (\(i, b) -> i * (Unsafe.fromJust (Map.lookup b bagSizeMap))) l)) m
  in Unsafe.fromJust $ Map.lookup name bagSizeMap

-- * FIRST problem
ex0 = parseContent [fmt|light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.|]


howManyCanContain bagName m = length (filter identity (Map.elems bagContainMap))
  where
    bagContainMap = Map.map (\b -> containedIn b) m

    containedIn bag
      | bagName `elem` (map snd bag) = True
      | otherwise = any (\n -> (Unsafe.fromJust $ Map.lookup n bagContainMap)) (map snd bag)


day :: _ -> Int
day = howManyCanContain ("shiny gold")


-- * SECOND problem
ex1 = parseContent [fmt|shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.|]

day' :: _ -> Int
day' = subtract 1 . bagSize ("shiny gold")

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 4
    it "of second star" $ do
      day' ex1 `shouldBe` 126
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 316
    it "on second star" $ do
      day' fileContent `shouldBe` 11310
