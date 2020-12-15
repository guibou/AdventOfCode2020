module Day15 where

import Utils
import qualified Relude.Unsafe as Unsafe
import Relude.Extra
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Megaparsec

-- start: 17:23 -> 17:47 -> 18:04

fileContent :: [Int]
fileContent = [2,0,6,12,1,3]

-- * Generics


-- * FIRST problem
algo init n = go (n - length init) (length init + 1) (Unsafe.last init) (Map.fromList (zip (take (length init - 1) init) [1..]))
  where

go 0 _ lastSpokenNumber _ = lastSpokenNumber
go n (traceShowId -> !turnNumber) !lastSpokenNumber mapLastNumber = g
      where
        !l = case Map.lookup lastSpokenNumber mapLastNumber of
          Nothing -> 0
          Just lastTimeItWasSaid -> turnNumber - lastTimeItWasSaid - 1
        g = go (n - 1) (turnNumber + 1) l (Map.insert lastSpokenNumber (turnNumber - 1) mapLastNumber)



-- * SECOND problem
day content n = algo content n

-- * Tests

-- test :: Spec
-- test = do
--   describe "simple examples" $ do
--     it "of first star" $ do
--       day "" `shouldBe` 0
--     it "of second star" $ do
--       day' "" `shouldBe` 0
--   describe "works" $ do
--     it "on first star" $ do
--       day fileContent `shouldBe` 1228
--     it "on second star" $ do
--       day' fileContent `shouldBe` 1238
