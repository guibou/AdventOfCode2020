{-# LANGUAGE GADTs #-}
module Day23 where

import Utils
import qualified Data.IntMap as IntMap

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
data Status = Status
  Int
  -- ^ The current cup.
  [Int]
  -- ^ Here we have a backward list containing the value already seen.
  [Int]
  -- ^ Here we have the rest of the list, not yet consumed values.
  (IntMap [Int])
  -- ^ This are the "picked" triples, which need to be inserted back after a value.
  deriving (Show)

startStatus (x:xs) = Status x [] xs IntMap.empty
startStatus [] = error "Empty init list"

popOne :: Status -> (Status, Int)
popOne (Status current seen [] waitingPicked) = popOne (Status current [] (reverse seen) waitingPicked)
popOne (Status current seen (x:xs) waitingPicked) = case IntMap.lookup x waitingPicked of
  Nothing -> (Status current seen xs waitingPicked, x)
  Just picked -> (Status current seen (picked <> xs) (IntMap.delete x waitingPicked), x)

step :: Int -> Status -> Status
step m status = let
  (s', p0) = popOne status
  (s'', p1) = popOne s'
  (Status current seen rest waitingPicked, p2) = popOne s''

  picked = [p0, p1, p2]
  dest = findDestination m (current - 1) picked

  status' = Status current seen rest (IntMap.insertWith (error "duplicate") dest picked waitingPicked)

  in pickNextCup status'

pickNextCup :: Status -> Status
pickNextCup (popOne -> (Status current seen nextItems waitingPicked, current')) = Status current' (current:seen) nextItems waitingPicked

finishStatus s@(Status current seen nextItems waitingPicked)
  | null waitingPicked = current:nextItems <> reverse seen
  | otherwise = finishStatus (pickNextCup s)

-- *

findDestination m 0 picked = findDestination m m picked
findDestination m t picked
  | not (t `elem` picked) = t
  | otherwise = findDestination m (t - 1) picked

score (finishStatus -> l) = let
  (prefix, (_:suffix)) = break (==1) l
  in mconcat $ map show $ suffix <> prefix


-- * FIRST problem
ex0 :: [Int]
ex0 = parseContent 389125467

day :: _ -> String
day = score . applyN 100 (step 9) . startStatus

-- * SECOND problem
oneMillion = 1000000
tenMillion = 10 * oneMillion

day' :: [Int] -> Int
day' l = score' $ applyN tenMillion (step oneMillion) (startStatus $ l <> [10..oneMillion])

score' (finishStatus -> l) = go (cycle l)
  where
    go (1:x:y:_) = x * y
    go (_:xs) = go xs
    go _ = error "Working on an empty list, that's surprising"

-- * Tests
test :: Spec
test = do
  describe "simple examples" $ do
    it "of first star" $ do
      day ex0 `shouldBe` "67384529"
    it "of second star" $ do
      day' ex0 `shouldBe` 149245887792
  describe "works" $ do
    it "on first star" $ do
      day fileContent `shouldBe` "28946753"
    it "on second star" $ do
      day' fileContent `shouldBe` 519044017360
