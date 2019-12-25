{-|
Module      : Core.Selection.Internals
Description : Internal implementation for evaluating, sorting and choosing best candidates of a list of paths.
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com

For clarification, throughout this documentation the term "input-paths" will refer to a list of paths from a given graph. 
Usually this list of paths are the valid paths of a graph. 

The term "candidates" is used for the list of paths which are considered a result of the opinosis summary.
If I want an opinosis summary of a huge graph with 3 sentences, the input-paths can be very huge, but each candidate will have a length of 3. 

This module is the computationally most intensive of this library.

The computation times are still highly dependent on the given metrics and distance functions, see "Core.Metric" for this. 
-}
module Core.Selection.Internals where 

import Core.Types 

import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Map.Strict as SMap
import Data.Text (Text(..))
import Data.Sort (sortOn)
import Data.List.Ordered(nubSort)
import qualified Data.Set as Set


-- | returns the n paths which are - in regard to each other - achieving the highest overall score
-- 
-- The score is calculated by multiplying the metric values and the distances between paths.
-- 
-- If more paths are requested then being put in, the paths are returned.
bestPaths :: 
    Metric -- ^ The metric to calculate the value of a single path
    -> DistanceFunction -- ^ the distance function, which compares two paths 
    -> Word -- ^ the number of paths which should be returned 
    -> Double -- ^ sigmaDelta, threshold which the metric for each path has to superset
    -> [Path] -- ^ input paths - all possible paths, regardless of their metrics and distances
    -> [Path] 
bestPaths mFn distFn n theta ps = 
    let 
        n' = min (fromIntegral n) (length ps) -- There was a bug where one could not select 3 best paths of 2 best paths
        candidates = validCandidatesWithLength ps (fromIntegral n')
        filteredCandidates = filterBySigmaTheta candidates mFn theta
        sortedCandidates = sortByOverallValue mFn distFn filteredCandidates
    in
        getFirst sortedCandidates
    where 
        getFirst :: [[Path]] -> [Path]
        getFirst [] = []
        getFirst (x:_) = x

-- | Returns the n-long result candidates of a list of paths. 
-- 
-- example (a,b,c,d are paths): 
--
-- @
-- validCandidatesWithLength [a,b,c,d] 3 = [[a,b,c],[b,c,d],[c,d,a],[d,a,b]]
-- @
--
-- The candidates will be n-long and unique. if [a,b] is in the candidates [b,a] will not be in the candidates.
-- 
-- In case n is 3, but there are only 2 input-paths, an empty list will be returned. 
--
-- Note: The initial implementation was producing the superset of paths, checking their length for n, and return the set of it. 
-- This had fatal (read: truly fatal) performance impacts. Do not do this.
validCandidatesWithLength :: 
    [Path] -- ^ Input Paths - usually all validPaths of an opinosis-graph
    -> Word  -- ^ Number of result-sentences, will be the size of the returned result-candidates
    -> [[Path]] -- ^ List of the Result candidates
validCandidatesWithLength ps n = ntuples ps n 
    where
        candidatesWithLength :: Word -> [Path] -> [[Path]]
        candidatesWithLength 0 _ = []
        candidatesWithLength 1 xs = (:[]) <$> xs
        candidatesWithLength n ps = let lastIt = candidatesWithLength (n-1) ps
                                    in [p : xs | p <- ps, xs <- lastIt]
        uniqueCandidates :: [[Path]] -> [[Path]]
        uniqueCandidates = map Set.toList . Set.toList . Set.fromList . map Set.fromList

-- | Given a distance-function, all distances between the paths are returned.
-- The distances will be returned as a list and are already unique (e.g. if dist(a,b) is already in the list, dist(b,a) will not be in the output)
calculateAllDistances :: DistanceFunction -> [Path] -> [Double]
calculateAllDistances _ [] = []
calculateAllDistances _ [x] = [0.0]
calculateAllDistances dist (x:os) = map (dist x) os ++ calculateAllDistances dist os

-- | Inverts the distances
--
-- If I don't invert them, I will get the closest sentences instead of the most distinct
invertedDistances dFn ps = [1-u | u <- calculateAllDistances dFn ps]

-- | applies the metric function to a list of paths
calculateAllMetrics :: Metric -> [Path] -> [Double]
calculateAllMetrics = map

-- | Calculates the value of a List of Paths.
-- 
-- This is used to calculate the value of a result candidate for sorting.
--
-- This means calculating all metrics, all distances and multiplying them.
-- The "invertedDistances" function is used (not normal distances).
overAllValue :: Metric -> DistanceFunction -> [Path] -> Double
overAllValue _ _ [] = 0.0
overAllValue mFn _ [x] = mFn x
overAllValue mFn dFn ps = product ((mFn <$> ps) ++ invertedDistances dFn ps)

-- | Checks a list of candidates whether they are over the sigmaDelta threshold given a metric 
filterBySigmaTheta :: 
    [[Path]] -- ^ list of result-candidates 
    -> Metric -- ^ metric to evaluate a candidate
    -> Double  -- ^ sigmaDelta threshhold, which result-candidates have to reach
    -> [[Path]] -- ^ all candidates, which have metric(candidate)>sigmaDelta
filterBySigmaTheta ps mfn theta = filter (\x -> sigmaThetaQualified x mfn theta) ps
    where 
        sigmaThetaQualified :: [Path] -> Metric -> Double -> Bool
        sigmaThetaQualified p mfn sigma= product (map mfn p) > sigma

-- | sorts the candidates by their overall Value
-- 
-- The most distinct tuples which have the highest metric-values are the first in the list.
-- 
-- Does not shorten the list of candidates, does not filter the list of candidates in any way.  
sortByOverallValue :: Metric -> DistanceFunction -> [[Path]] -> [[Path]]
sortByOverallValue mFn dFn = sortOn (overAllValue mFn dFn)

-- | Creates unique ntuples of a list. 
-- 
-- The List needs to be sortable, but does not need to be sorted. 
-- The List will be made unique and the resulting tuples should also be sorted. 
--
-- It has simple error handling for empty lists, to short tuples and to small / big ints
ntuples ::(Ord a) => [a] -> Word -> [[a]]
ntuples as n = if length as < fromIntegral n 
               then []
               else go n as' 
    where
        as' = nubSort as 
        go :: (Ord b) => Word -> [b] -> [[b]]
        go 0 _      = [[]]  -- There is only one "0-tuple" = empty list
        go _ []     = []    -- It's impossible to make n-tuple (n>0) out of empty list
        go n (x:xs) = [ x:ys | ys <- go (n-1) xs ] ++ go n xs
