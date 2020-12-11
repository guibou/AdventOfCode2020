module Day08 where

import Utils
import qualified Relude.Unsafe as Unsafe
import Text.Megaparsec
import qualified Data.Set as Set

-- start 18:43
-- star 1: 18:52
-- star 2: 18:59

data OpCode = Jmp | Nop | Acc
  deriving Show

data Instr = Instr OpCode !Int
  deriving (Show)

parseOp = lexeme $ choice [
  "acc" $> Acc,
  "jmp" $> Jmp,
  "nop" $> Nop
  ]

parseInstr = do
  op <- parseOp
  n <- parseNumber

  pure $ Instr op n

fileContent :: _
fileContent = parseContent $(getFile)

parseContent :: Text -> _
parseContent = unsafeParse (Text.Megaparsec.some parseInstr)

-- * Generics
data Machine = Machine Int Int
  deriving (Show)

data State = Loop Int | Terminate Int
  deriving (Show, Eq)

run code = go (Machine 0 0) Set.empty
  where
    go m@(Machine pc acc) visited
      | pc == length code = Terminate acc
      | pc `Set.member` visited = Loop acc
      | otherwise = go (step m (code Unsafe.!! pc)) (Set.insert pc visited)

step (Machine pc acc) (Instr op d) = let
  (dpc, dacc) = case op of
    Nop -> (1, 0)
    Acc -> (1, d)
    Jmp -> (d, 0)
  in Machine (pc + dpc) (acc + dacc)


-- * FIRST problem
ex0 = parseContent [fmt|nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6|]

day = run

-- * SECOND problem
alternateCodes [] = [[]]
alternateCodes (Instr x d:xs) = let
  alts = alternateCodes xs
  in case x of
  Acc -> ((Instr Acc d):) <$> alts
  Jmp -> (((Instr Jmp d):) <$> alts) <> (((Instr Nop d):) <$> [xs])
  Nop -> (((Instr Jmp d):) <$> [xs]) <> (((Instr Nop d):) <$> alts)


isTerminate (Terminate _) = True
isTerminate _ = False

day' = find isTerminate . fmap run . alternateCodes

-- * Tests

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` Loop 5
    it "of second star" $ do
      day' ex0 `shouldBe` Just (Terminate 8)
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` Loop 1723
    it "on second star" $ do
      day' fileContent `shouldBe` Just (Terminate 846)
