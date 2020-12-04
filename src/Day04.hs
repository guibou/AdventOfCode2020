module Day04 where

import Utils
import Text.Megaparsec
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set

-- srat 16:22
-- star at 16:38
-- 17:09

fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent c = unsafeParse (parsePassport `sepBy` "@") (Text.replace "\n\n" "@" c)

parseField = do
  name <- Text.Megaparsec.some $ choice (map single ['a'..'z'])
  void ":"
  value <- Text.Megaparsec.some $ choice (map single (['a'..'z'] <> ['0'..'9'] <> "#"))

  pure $ (Text.pack name, Text.pack value)

parsePassport = Map.fromList <$> do
  parseField `sepBy` choice [" ", "\n"]

mandatoryFields = Set.fromList $ words "byr iyr eyr hgt hcl ecl pid"

isValidPassport p = null $ mandatoryFields `Set.difference` Map.keysSet p

-- * Generics


-- * FIRST problem
day :: _ -> Int
day = length . filter isValidPassport

-- * SECOND problem

-- byr (Birth Year) - four digits; at least 1920 and at most 2002.

validByr v = case readMaybe @Int (Text.unpack v)of
  Just n -> n >= 1920 && n <= 2002
  Nothing -> False

validIyr v = case readMaybe @Int (Text.unpack v)of
  Just n -> n >= 2010 && n <= 2020
  Nothing -> False

validEyr v = case readMaybe @Int (Text.unpack v)of
  Just n -> n >= 2020 && n <= 2030
  Nothing -> False

validHgt v = let
  n = Text.take (Text.length v - 2) v
  e = Text.drop (Text.length v - 2) v

  in case e of
       "cm" -> case readMaybe @Int (Text.unpack n) of
         Just n' -> n' >= 150 && n' <= 193
         Nothing -> False
       "in" -> case readMaybe @Int (Text.unpack n) of
         Just n' -> n' >= 59 && n' <= 76
         Nothing -> False
       _ -> False

validHcl (Text.unpack -> ('#':xs)) = all isDigitAf xs
  where
    isDigitAf c = c `elem` ['a'..'f'] || isDigit c
validHcl _ = False

validEcl v = v `Set.member` (Set.fromList $ words "amb blu brn gry grn hzl oth")

validPid v = case readMaybe @Int (Text.unpack v) of
  Just _ -> Text.length v == 9
  Nothing -> False

validate (t, v) = (case t of
        "byr" -> validByr
        "iyr" -> validIyr
        "eyr" -> validEyr
        "hgt" -> validHgt
        "hcl" -> validHcl
        "ecl" -> validEcl
        "pid" -> validPid
        "cid" -> const True
        e -> error ("WTF" <> e)
           ) v

validatePassport :: Map Text Text -> Bool
validatePassport v = isValidPassport v && all validate (Map.toList $ v)

-- 203 too high
day' = length . filter validatePassport

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 2
    it "of second star" $ do
      day' invalids `shouldBe` 0
      day' valids `shouldBe` 4
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 226
    it "on second star" $ do
      day' fileContent `shouldBe` 160

ex0 = parseContent [fmt|\
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in|]


invalids = parseContent [fmt|\
eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007|]

valids = parseContent [fmt|\
pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719|]
