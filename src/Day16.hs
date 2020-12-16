module Day16 where

import Utils
import qualified Data.List
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Data.Map as Map
import Text.Megaparsec

-- start: 16:51. 17:20. 18:16

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent t = let
  [rulesT, ticketT, nearbyTicketT] = Text.splitOn "\n\n" t
  in (
     unsafeParse (Text.Megaparsec.some parseRule) rulesT,
     unsafeParse ("your ticket:\n" *> parseTicket) ticketT,
     unsafeParse ("nearby tickets:\n" *> parseTicket `sepBy` "\n") nearbyTicketT
     )

data Rule = Rule Text Range Range
  deriving (Show)

data Range = Range Int Int
  deriving (Show)

inRange v (Range a b) = v >= a && v <= b

parseRule = do
  ruleName <- Text.pack <$> someTill (anySingle) ": "
  r1 <- parseRange
  void $ "or "
  r2 <- parseRange

  pure $ Rule ruleName r1 r2

parseRange = do
  d <- parseNumber
  void "-"
  d' <- parseNumber
  pure $ Range d d'

-- * Generics
parseTicket = parseNumber' `sepBy` ","

parseNumber' = Unsafe.read @Int <$> Text.Megaparsec.some (choice (map single ['0'..'9']))

parseProblem = do
  rules <- Text.Megaparsec.some parseRule

  void "your ticket:\n"
  myTicket <- parseTicket
  void "nearby tickets:\n"
  nearbyTickets <- Text.Megaparsec.some parseTicket

  pure (rules, myTicket, nearbyTickets)

-- * FIRST problem
ex0 = parseContent [fmt|\
class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12|]

day :: _ -> Int
day (rules, _, tickets) = sum $ filter (\value -> not (any (inRange value) allRanges)) values
  where
   allRanges = do
     Rule _ r1 r2 <- rules
     [r1, r2]
   values = concat tickets

isValidTicket ranges values = all (\value -> any (inRange value) ranges) values

-- * SECOND problem
ex1 = parseContent [fmt|\
class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9|]

countValid tk rule = length (filter (checkRule rule) tk)

day' :: _ -> Maybe Int
day' (rules', myticket, unknownTickets) = (product . map fst . filter (\(_, Rule name _ _) -> "departure" `Text.isPrefixOf` name) . zip myticket . map snd . sortOn fst) <$> res
 where
  allRanges = do
    Rule _ r1 r2 <- rules'
    [r1, r2]

  tktIndex = Map.fromList (zip validTickets [0 :: Int ..])

  validTickets = transpose $ myticket : (filter (isValidTicket allRanges) unknownTickets)

  countsValid rules tickets = zip (map (countValid tickets) rules ) rules

  go [] [] = [[]]
  go rules tickets = case map snd $ sortOn fst (countsValid rules tickets) of
    [] -> [[]]
    (ruleValid:rs) -> do
      onlyValidforThisRule <- filter (checkRule ruleValid) tickets

      let idx = Map.lookup onlyValidforThisRule tktIndex

      ((idx, ruleValid) :) <$> go rs (Data.List.delete onlyValidforThisRule tickets)

  res = viaNonEmpty head $ go rules' validTickets

checkRule :: Rule -> [Int] -> Bool
checkRule (Rule _ r1 r2) values = all (\x -> inRange x r1 || inRange x r2) values

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 71
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 26026
    it "on second star" $ do
      day' fileContent `shouldBe` Just 1305243193339
