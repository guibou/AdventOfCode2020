{-# LANGUAGE GADTs #-}
module Day23 where

import Relude.Unsafe as Unsafe
import Utils
import qualified Data.Vector.Unboxed.Mutable as VM

-- start: 14:39. 15:03
-- Second star, stuck in a stupid O(n*n) algo. Stopped at 15:40.
-- Restart at 18:09. I had a run and some ideas
-- 18:33: second star

fileContent :: [Int]
fileContent = parseContent $ 586439172

parseContent v = go v []
  where
    go 0 acc = acc
    go n acc = let (a, b) = divMod n 10
               in go a (b : acc)

-- * Generics
-- Main solution
solve :: Int -> Int -> (_ -> IO a) -> [Int] -> IO a
solve reps size finisher l = do
  arr :: VM.IOVector Int32 <- VM.new (size + 1)

  for_ (zip l (Unsafe.tail l)) $ \(a, b) -> do
    VM.unsafeWrite arr a (fromIntegral b)

  VM.unsafeWrite arr (Unsafe.last l) (fromIntegral (if size == 9 then Unsafe.head l else 10))

  for_ [10..size] $ \i ->
    VM.unsafeWrite arr i (fromIntegral (i+1))

  when (size /= 9) $ do
    VM.unsafeWrite arr size (fromIntegral $ Unsafe.head l)

  let
    go 0 _ = pure ()
    go n current = do
      p0 <- VM.unsafeRead arr (fromIntegral current)
      p1 <- VM.unsafeRead arr (fromIntegral p0)
      p2 <- VM.unsafeRead arr (fromIntegral p1)
      next <- VM.unsafeRead arr (fromIntegral p2)

      let
        dest = findDestination (fromIntegral size) (current - 1) (p0, p1, p2)

      destNext <- VM.unsafeRead arr (fromIntegral dest)

      -- Rechain pickle
      VM.unsafeWrite arr (fromIntegral dest) p0
      VM.unsafeWrite arr (fromIntegral p2) destNext

      -- Skip pickle
      VM.unsafeWrite arr (fromIntegral current) next

      go (n - 1) next

  go reps (fromIntegral $ Unsafe.head l)

  finisher arr

-- *
findDestination :: Int32 -> Int32 -> (Int32, Int32, Int32) -> Int32
findDestination m 0 picked = findDestination m m picked
findDestination m t picked@(a, b, c)
  | t /= a && t /= b && t /= c = t
  | otherwise = findDestination m (t - 1) picked

-- * FIRST problem
ex0 :: [Int]
ex0 = parseContent 389125467

day l = concatMap show <$> solve 100 9 finisher l
  where
    finisher :: VM.IOVector Int32 -> IO [Int]
    finisher arr = do
      let
        go 1 = pure []
        go current = do
          next <- VM.unsafeRead arr (fromIntegral current)
          res <- go next
          pure (fromIntegral current:res)


      go =<< VM.unsafeRead arr 1


-- * SECOND problem
oneMillion = 1000000
tenMillion = 10 * oneMillion

day' :: [Int] -> IO Int
day' = solve tenMillion oneMillion $ \arr -> do
  p0 <- VM.unsafeRead arr 1
  p1 <- VM.unsafeRead arr (fromIntegral p0)

  pure $ fromIntegral p0 * fromIntegral p1

-- * Tests
test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldReturn` "67384529"
    it "of second star" $ do
      day' ex0 `shouldReturn` 149245887792
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldReturn` "28946753"
    it "on second star" $ do
      day' fileContent `shouldReturn` 519044017360
