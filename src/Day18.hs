module Day18 where

import Utils
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as Text
import Text.Megaparsec
import Control.Monad.Combinators.Expr

-- start 15:33. 15:41. 15:43
expr table = makeExprParser term table <?> "expression"
  where
    expr' = expr table

    parens :: _ -> _
    parens e = symbol "(" *> e <* symbol ")"

    integer = Unsafe.read <$> Prelude.some (choice (map single ['0'..'9']))

    term = parens expr' <|> lexeme integer <?> "term"

binary  name f = InfixL  (f <$ symbol name)

fileContent :: _
fileContent = Text.splitOn "\n" $(getFile)

-- * Generics
solve table = sum . map (unsafeParse $ expr table)

-- * FIRST problem
day :: [Text] -> Int
day = solve [[binary "+" (+), binary "*" (*)]]

-- * SECOND problem
day' :: [Text] -> Int
day' = solve [[binary "+" (+)], [binary "*" (*)]]

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ["((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"] `shouldBe` 13632
    it "of second star" $ do
      day' ["((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"] `shouldBe` 23340
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` 45840336521334
    it "on second star" $ do
      day' fileContent `shouldBe` 328920644404583
