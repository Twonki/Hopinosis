module Core.Selection.Internals(bestPaths,validCandidatesWithLength) where 

import Core.Types 

import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Map.Strict as SMap
import Data.Text (Text(..))
import Data.Sort (sortOn)
import qualified Data.Set as Set

{- Tuple Selection -}
bestPaths :: Metric -> DistanceFunction -> Int -> Double -> [Path] -> [Path]
bestPaths mFn distFn n theta ps = 
    let 
        n' = min n (length ps) -- There was a bug where one could not select 3 best paths of 2 best paths
        candidates = validCandidatesWithLength ps n'
        filteredCandidates = filterBySigmaTheta candidates mFn theta
        sortedCandidates = sortByOverallValue mFn distFn filteredCandidates
    in
        getFirst sortedCandidates
    where 
        getFirst :: [[Path]] -> [Path]
        getFirst [] = []
        getFirst (x:_) = x


validCandidatesWithLength :: [Path] -> Int -> [[Path]]
validCandidatesWithLength ps n = filter (\c -> length c == n) $ uniqueCandidates $ candidatesWithLength n ps
    where
        candidatesWithLength :: Int -> [Path] -> [[Path]]
        candidatesWithLength 0 _ = []
        candidatesWithLength 1 xs = (\x -> [x]) <$> xs
        candidatesWithLength n ps = let lastIt = candidatesWithLength (n-1) ps
                                    in [[p] ++ xs | p <- ps, xs <- lastIt]
        uniqueCandidates :: [[Path]] -> [[Path]]
        uniqueCandidates = (map Set.toList) . Set.toList . Set.fromList . (map Set.fromList)

calculateAllDistances :: DistanceFunction -> [Path] -> [Double]
calculateAllDistances _ [] = []
calculateAllDistances _ (x:[]) = [0.0]
calculateAllDistances dist (x:os) = map (dist x) os ++ calculateAllDistances dist os

-- As I want to highest distance between sentences
-- If I don't invert them, I will get the closest sentences instead of the most distinct
invertedDistances dFn ps = [1-u | u <- calculateAllDistances dFn ps]

calculateAllMetrics :: Metric -> [Path] -> [Double]
calculateAllMetrics = map

overAllValue :: Metric -> DistanceFunction -> [Path] -> Double
overAllValue _ _ [] = 0.0
overAllValue mFn _ (x:[]) = mFn x
overAllValue mFn dFn ps = product ((mFn <$> ps) ++ (invertedDistances dFn ps))

filterBySigmaTheta :: [[Path]] -> Metric -> Double -> [[Path]]
filterBySigmaTheta ps mfn theta = filter (\x -> sigmaThetaQualified x mfn theta) ps
    where 
        sigmaThetaQualified :: [Path] -> Metric -> Double -> Bool
        sigmaThetaQualified p mfn sigma= product (map mfn p) > sigma


sortByOverallValue :: Metric -> DistanceFunction -> [[Path]] -> [[Path]]
sortByOverallValue mFn dFn ps = sortOn (overAllValue mFn dFn) ps