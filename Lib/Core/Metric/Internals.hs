{-|
Module      : Core.Metric
Description : Contains helpers and simple metrics to evaluate paths
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com

This module contains example metrics - however they are not required for the overall procedure. 

It is only here to provide example values and for the "commonBestPaths".
-}
module Core.Metric.Internals where 

import Core.Node   
import Core.Types

import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Map.Strict as SMap
import Data.Text (Text)
import Data.Sort (sortOn)
import Data.Monoid(Sum(..),Any(..))

import qualified Data.List.Ordered as Ordered

import qualified Data.Set as Set

import Control.Parallel


{-
* Meta Functions

$MetaFunctions

The following functions are "meta-functions" regarding metrics, such as combining or inverting metrics. 
-}

-- | Returns a function, which sums the results of two metric functions. 
metricAdd :: Metric -> Metric -> Metric 
metricAdd a b p = a p + b p

-- | Returns a function, which multiplies the results of two metric functions. 
metricProd :: Metric -> Metric -> Metric 
metricProd a b p = a p * b p


-- | Multiplies the results of two distance function. 
--
-- This should keep the required attributes such as symmetry and reflexivity of distance functions, 
-- iff both distance functions keep these attributes. 
--
-- Thats why there is no distance-add, as e.g. the reflexivity would be either violated 
-- or the inputs where no valid distance functions.
distanceProd :: DistanceFunction -> DistanceFunction -> DistanceFunction
distanceProd a b p1 p2 = a p1 p2 * b p1 p2 

{-
* Metric Functions

$MetricFunctions

-}

-- | Calculates the accumulated strength of the edges of a path. 
-- 
-- This is done via a simple lookup and addition.
--
-- Most prominent connections will yield the highest metric, no matter how popular their nodes are.
-- However, The magnitude can never be lower than the edgeStrength.
edgeStrengths :: Metric
edgeStrengths [] = 0.0
edgeStrengths [_] = 0.0
edgeStrengths p@(_:y:xs)= fromIntegral (nextStrength p) + edgeStrengths (y:xs)
    where 
        nextStrength :: Path -> Word 
        nextStrength [] = 0
        nextStrength [_] = 0
        nextStrength (u:v:_) = let (Sum a) = outs (snd u) MMap.! fst v in a 

-- | Averaged out strength, first runs "edgeStrengths" and divides it by path length.
averagedEdgeStrengths :: Metric
averagedEdgeStrengths p = edgeStrengths p / fromIntegral (length p)

-- | Calculates the accumulated magnitudes of a path.
--
-- Most prominent words will yield the highest metric, no matter how they are connected. 
magnitudes :: Metric 
magnitudes p = let (Sum b) = mconcat (magnitude .snd <$> p) in fromIntegral b

-- | Averaged Magnitudes, first runs "magnitudes" and divides by path length
averagedMagnitudes :: Metric 
averagedMagnitudes p = magnitudes p / fromIntegral (length p)

{-
* Distance Functions

$DistanceFunctions

-}

-- | Calculates consine similarity for two paths
--
-- Source: https://en.wikipedia.org/wiki/Cosine_similarity
-- 
-- This is a candidate to be replaced with a library.
cosineSim :: Path -> Path -> Double
cosineSim [] _ = 0.0
cosineSim _ [] = 0.0
cosineSim p1 p2 = 
    if p1 == p2 
    then 1.0 -- Shortcut equal items
    else cosine p1vec p2vec
        where 
            (p1vec,p2vec) = toVectors (p1,p2)
            cosine :: [Double] -> [Double] -> Double
            cosine xs ys = dot xs ys / (len xs * len ys)
                where 
                    dot a b = sum $ zipWith (*) a b
                    len a = sqrt $ dot a a


-- | Calculates the jaccard similarity of two paths
--   
-- Source: https://en.wikipedia.org/wiki/Jaccard_index
jaccardSim :: Path -> Path -> Double 
jaccardSim [] _ = 0.0
jaccardSim _ [] = 0.0
jaccardSim p1 p2 = 
            if p1' == p2' 
            then 1.0
            else fromIntegral (length hits) / fromIntegral (length unio)
            where 
                p1' = Ordered.nubSort $ map fst p1 
                p2' = Ordered.nubSort $ map fst p2 
                hits = Ordered.isect p1' p2'
                unio = Ordered.union p1' p2'
{-
* Helper Functions

$HelperFunctions

-}

-- | Helper Function to make a one-hot encoded vectors
-- 
-- @
-- toVectors ([a,b],[b,c]) = ([1,1,0],[0,1,1])
-- toVectors ([a,b,d],[a,c,d]) = ([1,1,0,1],[1,0,1,1])
-- @
--
-- Primarily required for "cosineSim".  
toVectors :: (Path,Path) -> ([Double],[Double])
toVectors (p1,p2) = 
        (map (\w -> SMap.findWithDefault 0 w p1') unionBagOfWords, map (\w -> SMap.findWithDefault 0 w p2') unionBagOfWords)
        where 
            unique = Set.toList . Set.fromList
            unionBagOfWords = unique $ (fst <$> p1) ++ (fst <$> p2)
            p1' = SMap.fromAscList $ (\(k,v) -> (k,fromIntegral $ getSum $ magnitude v)) <$> p1
            p2' = SMap.fromAscList $ (\(k,v) -> (k,fromIntegral $ getSum $ magnitude v)) <$> p2