{-|
Module      : Core.Metric
Description : Contains helpers and simple metrics to evaluate paths
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com

This module contains example metrics - however they are not required for the overall procedure. 

It is only here to provide example values and for the "commonBestPaths".

Functions are interfaced to "Core.Selection.Internals"
-}
module Core.Metric where 

import Core.Node   
import Core.Types

import qualified Core.Metric.Internals as Internals


{-
* Meta Functions

$MetaFunctions

The following functions are "meta-functions" regarding metrics, such as combining or inverting metrics. 
-}

-- | Returns a function, which sums the results of two metric functions. 
metricAdd :: Metric -> Metric -> Metric 
metricAdd = Internals.metricAdd

-- | Returns a function, which multiplies the results of two metric functions. 
metricProd :: Metric -> Metric -> Metric 
metricProd = Internals.metricProd


-- | Multiplies the results of two distance function. 
--
-- This should keep the required attributes such as symmetry and reflexivity of distance functions, 
-- iff both distance functions keep these attributes. 
--
-- Thats why there is no distance-add, as e.g. the reflexivity would be either violated 
-- or the inputs where no valid distance functions.
distanceProd :: DistanceFunction -> DistanceFunction -> DistanceFunction
distanceProd = Internals.distanceProd

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
edgeStrengths = Internals.edgeStrengths

-- | Averaged out strength, first runs "edgeStrengths" and divides it by path length.
averagedEdgeStrengths :: Metric
averagedEdgeStrengths = Internals.averagedEdgeStrengths

-- | Calculates the accumulated magnitudes of a path.
--
-- Most prominent words will yield the highest metric, no matter how they are connected. 
magnitudes :: Metric 
magnitudes = Internals.magnitudes

-- | Averaged Magnitudes, first runs "magnitudes" and divides by path length
averagedMagnitudes :: Metric 
averagedMagnitudes = Internals.magnitudes

{-
* Distance Functions

$DistanceFunctions

-}

-- | Calculates consine similarity for two paths
cosineSim :: Path -> Path -> Double
cosineSim = Internals.cosineSim


-- | Calculates the jaccard similarity of two paths
--   
-- Source: https://en.wikipedia.org/wiki/Jaccard_index
jaccardSim :: Path -> Path -> Double 
jaccardSim = Internals.jaccardSim
