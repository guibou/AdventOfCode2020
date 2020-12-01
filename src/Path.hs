{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
module Path
  (shortestPath
  , shortestPathAll
  )
where

import Protolude
import Utils

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)

import Data.Foldable (foldl')

import qualified Data.PQueue.Prio.Min as Queue

-- | find the shortest path in a graph
dijkstra ::
  forall v w. (Hashable v, Ord v, Ord w, Num w)
  => (v -> [(w, v)]) -- ^ transition function frow vertex *v* to new vertices associated with weight *w*.
  -> (w -> w -> w) -- ^ weight combination function, usually (+) for distances
  -> v -- ^ starting vertex
  -> Maybe v -- ^ end vertex
  -> HashMap v (w, v) -- ^ associate a vertex *v* with its weight from the starting vertex and its previous vertex
dijkstra getNext combineWeight start endM = go (Queue.singleton 0 start) HashMap.empty HashSet.empty
  where
    go :: Queue.MinPQueue w v -> HashMap v (w, v) -> HashSet v -> HashMap v (w, v)
    go queue prevs done =
      case Queue.minViewWithKey queue of
        Nothing -> prevs
        Just ((w, currentPoint), queue')
          -- shortcut computation if we are looking for the final point only
          | Just end <- endM, currentPoint == end -> prevs
          | currentPoint `HashSet.member` done -> go queue' prevs done
          | otherwise ->
            let
              nexts = getNext currentPoint
              nextPriority = map (\(w', v) -> (w' `combineWeight` w, v)) nexts

              -- update queue
              queue'' = foldl' (\acc (k, a) -> Queue.insert k a acc) queue' nextPriority
              -- update prevs
              upPrevs = HashMap.fromList (map (\(weight, v) -> (v, (weight, currentPoint))) nextPriority)

              fUnion p0@(weight, _) p1@(weight', _)
                | weight <= weight' = p0
                | otherwise = p1
              in go queue'' (HashMap.unionWith fUnion prevs upPrevs) (HashSet.insert currentPoint done)

-- | find the shortest path in a graph between two nodes. It ends computation once the node is found
shortestPath ::
  forall v w. (Hashable v, Show w, Ord v, Ord w, Num w)
  => (v -> [(w, v)]) -- ^ transition function frow vertex *v* to new vertices associated with weight *w*.
  -> (w -> w -> w) -- ^ weight combination function, usually (+) for distances
  -> v -- ^ starting vertex
  -> v -- ^ ending vertex
  -> Maybe (w, [v]) -- ^ the list of vertices of the path associated with the weight
shortestPath getNext combineWeight start end = let
  d = dijkstra getNext combineWeight start (Just end)
  in buildPath start end d

-- | Find the shortest paths between two nodes.
-- This function can be more efficient than `shortestPath` if you want
-- to find all the shortest paths from one node to many others. You
-- must partially apply it such as:
--
-- >>> pathsFrom0 = shortestPathAll f weightf 0
-- >>> pathTo1 = pathsFrom0 1
-- >>> pathTo2 = pathsFrom0 2
shortestPathAll ::
  forall v w. (Hashable v, Show w, Ord v, Ord w, Num w)
  => (v -> [(w, v)]) -- ^ transition function frow vertex *v* to new vertices associated with weight *w*.
  -> (w -> w -> w) -- ^ weight combination function, usually (+) for distances
  -> v -- ^ starting vertex
  -> v -- ^ ending vertex
  -> Maybe (w, [v]) -- ^ the list of vertices of the path associated with the weight
shortestPathAll getNext combineWeight start = let
  d = dijkstra getNext combineWeight start Nothing
  in \end -> buildPath start end d

buildPath ::
  (Hashable v, Show w, Ord v, Ord w, Num w)
  => v -- ^ starting vertex
  -> v -- ^ ending vertex
  -> HashMap v (w, v) -- ^ result of *dijkstra*
  -> Maybe (w, [v]) -- ^ resulting path with its weight
buildPath start end d
  | start == end = Just (0, [])
  | otherwise = case HashMap.lookup end d of
  Nothing -> Nothing
  Just (w, _prev) -> Just (w, go end [])
    where
      go current acc
        | current == start = acc
        | Just (_, prev) <- HashMap.lookup current d = go prev (current:acc)
        | otherwise = error "WTF buildPath"
