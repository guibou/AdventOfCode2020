module Day15 where

import Test.Hspec
import qualified Relude.Unsafe as Unsafe
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST (runST, ST)

-- start: 17:23 -> 17:47 -> 18:04

fileContent :: [Int]
fileContent = [2,0,6,12,1,3]

-- * Generics


-- * FIRST problem
algo' init n = runST $ do
  arr <- VM.replicate n (-1)

  for_ (zip (take (length init - 1) init) [1..]) $ \(lastValue, lastRound) -> do
    VM.unsafeWrite arr lastValue lastRound

  go' arr (n - length init) (length init + 1) (Unsafe.last init)

go' :: forall s. VM.STVector s Int32 -> Int -> Int -> Int -> ST s Int32
go' arr = gogo
  where
    gogo 0 !_ !lastSpokenNumber = pure $ fromIntegral $ lastSpokenNumber
    gogo !n !turnNumber !lastSpokenNumber = do
      (fromIntegral -> !lastTurn) <- VM.unsafeRead arr lastSpokenNumber
      VM.unsafeWrite arr lastSpokenNumber (fromIntegral $ turnNumber - 1)

      let
        !l
         | lastTurn == -1 = 0
         | otherwise = turnNumber - lastTurn - 1

      gogo (n - 1) (turnNumber + 1) l


-- * SECOND problem
day content n = algo' content n

main :: IO ()
main = do
  print (day fileContent 30000000)


-- * Tests
star1n = 2020
star2n = 30000000

test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day [1,3,2] star1n `shouldBe` 1
    it "of second star" $ do
      day [1,3,2] star2n `shouldBe` 2578
  describe "works" $ do
    it "on first star" $ do
      day fileContent star1n `shouldBe` 1428
    it "on second star" $ do
      day fileContent star2n `shouldBe` 3718541
