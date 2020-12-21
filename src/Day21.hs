module Day21 (test) where

import Utils
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec
import Data.List (delete)

-- start: 11:19.
-- first star: 12:09 (I did not understood it...)
-- second star: 12:24

fileContent :: _
fileContent = parseContent $(getFile)

parseIngredient = Text.Megaparsec.some (oneOf ['a'..'z'])

parseFood = do
  ingredients <- parseIngredient `endBy` " "
  void "(contains "
  allergens <- parseIngredient `sepBy` ", "
  void ")"

  pure $ (ingredients, allergens)

parseContent :: Text -> _
parseContent = unsafeParse (parseFood `sepBy` "\n")

-- * Generics


-- * FIRST problem
ex0 = parseContent [fmt|\
mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)|]

allIngrediants input = Set.fromList . concatMap fst $ input

allergentMustBe input = Map.toList $ Map.fromListWith (Set.intersection) $ do
  (ingredients, allergents) <- input

  allergent <- allergents

  pure (allergent, Set.fromList ingredients)

cannotPossiblyBeAllergent input = allIngrediants input `Set.difference` (Set.fromList $ concatMap (Set.toList . snd) $ allergentMustBe input)

countFoodOccurence input = Map.fromListWith (+) $ do
  (food, _) <- input
  f <- food
  pure (f, 1)

day :: _ -> Int
day input = sum (Map.elems $ Map.restrictKeys (countFoodOccurence input) (cannotPossiblyBeAllergent input))

-- * SECOND problem
removeNotAllergens input = map (\(food, al) -> (clean food, al)) input
  where
    notAllergent = cannotPossiblyBeAllergent input

    clean ingrediants = Set.toList (Set.fromList ingrediants `Set.difference` notAllergent)

recognizeAllergens (removeNotAllergens -> input) = go (Map.toList foo)
  where
    foo = Map.fromListWith Set.intersection $ do
      (food, als) <- input

      al <- als
      pure (al, Set.fromList food)

    go :: [(String, Set String)] -> [(String, String)]
    go [] = []
    go l = case find (\(_, foods) -> length foods == 1) l of
      Just k@(al, Set.toList -> [x]) -> (al, x) : go newL'
        where
          -- Remove this allergens

          newL :: [(String, Set String)]
          newL = Data.List.delete k l
          -- Remove this ingrediant
          newL' :: [(String, Set String)]
          newL' = map (\(a, b) -> (a, Set.delete x b)) newL
      _ -> error "Sad sad me"

day' input = intercalate "," $ map snd $ sortOn fst (recognizeAllergens input)

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` 5
    it "of second star" $ do
      day' ex0 `shouldBe` "mxmxvkd,sqjhc,fvjkl"
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 2176
    it "on second star" $ do
      day' fileContent `shouldBe` "lvv,xblchx,tr,gzvsg,jlsqx,fnntr,pmz,csqc"
