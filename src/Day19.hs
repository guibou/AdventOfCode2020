module Day19 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as Text
import qualified Data.Map as Map

-- start: 22:10. 22:58. Stop at 23:47. I'm really annoyed here.
-- restart: 10:40. 12:15

-- * Parsing

fileContent :: _
fileContent = parseContent $(getFile)

parseContent t = let
  (rules, Text.drop 2 -> texts) = Text.breakOn "\n\n" t
  content = Text.lines texts
  in (parseRules rules, content)

parseRules t = Map.fromListWith (\_ y -> y) $ map parseRule (Text.lines t)

data Rule = Single Char | Or [Rule] | And [Rule] | Sub Int
  deriving (Show)

parseRule :: Text -> (Int, Rule)
parseRule t =
  let
    (ruleNo, Text.drop 2 -> left) = Text.breakOn ": " t
    rules = Text.splitOn " | " left

  in (Unsafe.read @Int (Text.unpack ruleNo), evalRule rules)

evalRule :: [Text] -> Rule
evalRule t = case map (toRule . Text.unpack) t of
  [x] -> x
  xs -> Or xs

toRule :: String -> Rule
toRule ('"':c:'"':[]) = Single c
toRule l = case (do
  rId <- map (Unsafe.read @Int . Text.unpack) (Text.splitOn " " (Text.pack l))

  pure (Sub rId)
                ) of
             [x] -> x
             xs -> And xs



-- * Evaluation


matchRule :: Map Int Rule -> [(Text, [Rule])] -> Bool
matchRule _ (keepDone -> (_:_)) = True
matchRule m (trimInvalids -> l@(_:_)) = matchRule m (concatMap (progress m) l)
matchRule _ _ = False

progress m (t@(Text.uncons -> Just (tc,ts)), (r:rs)) = case r of
    Single c
      -- We apply the char, pop the rule, and continue
      | tc == c -> [(ts, rs)]
      -- The char does not apply, broken rule
      | otherwise -> []
    Sub rId -> case Map.lookup rId m of
      Nothing -> error "WTF"
      -- Continue to next rule
      Just rule -> [(t, rule:rs)]
    Or cases -> map (\nw -> (t, nw:rs)) cases
    And cases -> [(t, cases <> rs)]
progress _ _ = []

keepDone = filter isDone
  where
    isDone ("", []) = True
    isDone _ = False

trimInvalids = filter valid
  where
    -- Some rule to apply and no char
    valid ("", (_:_)) = False
    -- Done, that's invalid
    valid ("", []) = False
    -- There is still some char, and no rule
    valid (_, []) = False
    valid (_, _) = True


-- * FIRST problem
ex0 = parseContent [fmt|\
0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb|]

altRules = parseRules [fmt|\
8: 42 | 42 8
11: 42 31 | 42 11 31|]

day (rules, texts) = length (filter (\t -> matchRule rules [(t, [Sub 0])]) texts)

day' (rules, texts) = day (altRules <> rules, texts)

-- * SECOND problem
ex1 = parseContent [fmt|\
42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba|]

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 2
    it "of second star" $ do
      day' ex1 `shouldBe` 12
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 178
    it "on second star" $ do
      day' fileContent `shouldBe` 346
