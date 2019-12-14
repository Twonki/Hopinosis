{-|
Module      : Core.Metric
Description : Contains helpers and simple metrics to evaluate paths
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com

This module contains example metrics - however they are not required for the overall procedure. 

It is only here to provide example values and for the "commonBestPaths".
-}
module Core.Metric where 

import Core.Node   
import Core.Types

import qualified Data.Map.Monoidal.Strict as MMap
import qualified Data.Map.Strict as SMap
import Data.Text (Text(..))
import Data.Sort (sortOn)
import qualified Data.Set as Set


-- | Calculates the accumulated strength of the edges of a path. 
-- 
-- This is done via a simple lookup and addition.
--
-- Most prominent connections will yield the highest metric, no matter how popular their nodes are.
-- However, The magnitude can never be lower than the edgeStrength.
edgeStrengths :: Metric
edgeStrengths [] = 0.0
edgeStrengths [x] = 0.0
edgeStrengths p@(x:y:xs)= (fromIntegral $ nextStrength p) + edgeStrengths (y:xs)
    where 
        nextStrength :: Path -> Int 
        nextStrength [] = 0
        nextStrength [x] = 0
        nextStrength (x:y:xs) = (outs $ snd x) MMap.! (fst y)

-- | Averaged out strength, first runs "edgeStrengths" and divides it by path length.
averagedEdgeStrengths :: Metric
averagedEdgeStrengths p = edgeStrengths p / (fromIntegral $ length p)

-- | Calculates the accumulated magnitudes of a path.
--
-- Most prominent words will yield the highest metric, no matter how they are connected. 
magnitudes :: Metric 
magnitudes p = fromIntegral (sum $ (magnitude .snd) <$> p)

-- | Averaged Magnitudes, first runs "magnitudes" and divides by path length
averagedMagnitudes :: Metric 
averagedMagnitudes p = magnitudes p / (fromIntegral $ length p)

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
            p1' = SMap.fromAscList $ (\(k,v) -> (k,fromIntegral $ magnitude v)) <$> p1
            p2' = SMap.fromAscList $ (\(k,v) -> (k,fromIntegral $ magnitude v)) <$> p2

-- | Calculates consine similarity for two paths
cosineSim :: Path -> Path -> Double
cosineSim [] _ = 0.0
cosineSim _ [] = 0.0
cosineSim p1 p2 = cosine p1vec p2vec
    where 
        (p1vec,p2vec) = toVectors (p1,p2)
        cosine :: [Double] -> [Double] -> Double
        cosine xs ys = dot xs ys / (len xs * len ys)
            where 
                dot a b = sum $ zipWith (*) a b
                len a = sqrt $ dot a a
