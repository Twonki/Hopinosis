{-|
Module      : Core.Selection
Description : Contains the functions to select the best n of given paths
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com

"commonBestPaths" provide a simple default values, or take a metric and a distance function.

Functions are interfaced to "Core.Selection.Internals"
-}
module Core.Selection(
    bestPaths,
    commonBestPaths
    ) 
where 

import qualified Core.Selection.Internals as Internals
import qualified Core.Metric as Metric

import Core.Types

-- | Interface to "Core.Selection.Internals"
bestPaths = Internals.bestPaths

-- | returns the best possible path creation given a set of default values
--
-- For the path-magnitude averagedEdgestrengths was chosen. 
-- 
-- For the similarity the cosine-similarity was chosen. 
-- 
-- The default number of paths to return is 3. 
--
-- The default required strength is 0.501, which given the averadgedEdgeStrength Metric.
-- This means that the edges have to be in more sentences than they are not.  
commonBestPaths :: [Path] -> [Path]
commonBestPaths = bestPaths Metric.averagedEdgeStrengths Metric.cosineSim 3 0.501