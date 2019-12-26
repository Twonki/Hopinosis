{-|
Module      : Hopinosis
Description : Toplevel Functions for the Opinosis Algorithm
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com

This Module contains several functions to make and use the Opinosis summaries from strings.
-}
module Hopinosis where 

import Core.Graph
import qualified Core.Metric as Metric
import Core.Path
import Core.Selection
import Core.Types

import Data.List.Split(endByOneOf)
import qualified Data.Text as Txt
import Data.Sort (sortOn)

-- |parses one sentence to an opinosis graph.    
toGraphOne :: String -> Graph
toGraphOne s =  parseSentence $ Txt.pack <$> words s

-- |parses a list of sentences to an opinosis graph.
toGraphMany :: [String] -> Graph
toGraphMany s = parseDocument (map Txt.pack . words <$> s)

-- |parses an un-split multisentence-text to an opinosis graph.
toGraphSentences:: String -> Graph 
toGraphSentences =  parseDocument . map (map (Txt.toLower . Txt.pack) . words) . endByOneOf ".;:!?\n"

-- |forms a readable sentence from a path.
toString :: Path -> String
toString p = Txt.unpack (pathJoin (fst <$> p))
    where
        pathJoin = Txt.intercalate (Txt.pack " ")

-- | A wrapped form of summarize.
-- 
-- Default values for "summarize" to make a shorter function.
commonSummarize :: String -> [String]
commonSummarize  = summarize Metric.averagedEdgeStrengths Metric.cosineSim 2 0.01 0.5 

-- | This function creates an opinosis summary. 
-- 
-- This is the primary function of the module.
-- 
-- The number of result-sentences greatly impacts the performance. 
summarize :: 
    Metric              -- ^ The Metric to evaluate a single path
    -> DistanceFunction -- ^ The Function to measure the distance between two sentences
    -> Int              -- ^ The Number of result-sentences
    -> Double           -- ^ The Sigma Alpha value, which threshhold for the number of starts must be met
    -> Double           -- ^ The Sigma Delta value, which threshhold for the metric needs to be met
    -> String           -- ^ The unsplit, multisentence-text
    -> [String]         -- ^ The Result-Sentences
summarize mFn dFn n alpha delta s = 
    let graph = toGraphSentences s
        paths = allPathsWithSigmaAlpha alpha graph
        bests = bestPaths mFn dFn n delta paths
    in toString <$> bests

-- | A less general summarize. 
-- 
-- Builds a graph, filters results by alpha and delta given the metric funcion, 
-- and returns the n best sentences given the metric. 
-- 
-- It can be expected to yield similiar results (read: repetitive sentence-parts). 
-- It can also be expected to be much faster than summarize with distances. 
-- 
-- An important part about this function is to (easily) compare the speed and results of 
-- "summarize". Maybe someone else can use a better approach to get distances to work faster. 
-- 
-- The size of n does not impact the performance. E.g. "small ns" won't increase performance 
-- unlike the "summarize" function. 
summarizeWithoutDistances :: 
    Metric              -- ^ The Metric to evaluate a single path
    -> Int              -- ^ The Number of result-sentences
    -> Double           -- ^ The Sigma Alpha value, which threshhold for the number of starts must be met
    -> Double           -- ^ The Sigma Delta value, which threshhold for the metric needs to be met
    -> String           -- ^ The unsplit, multisentence-text
    -> [String]         -- ^ The Result-Sentences
summarizeWithoutDistances mFn n alpha delta s =
    let graph    = toGraphSentences s 
        paths    = allPathsWithSigmaAlpha alpha graph 
        filtered = sortOn mFn (filter (\x -> mFn x >= delta) paths) 
    in toString <$> (take n filtered)

-- | a more general "summarize", where one can give the function to build the Graph. 
-- 
-- While currently unused, the idea is that potentially a graph can be load, read or otherwise created.
summarizeFrom :: 
    Metric                  -- ^ The Metric to evaluate a single path
    -> DistanceFunction     -- ^ The Function to measure the distance between two sentences
    -> Int                  -- ^ The Number of result-sentences
    -> Double               -- ^ The Sigma Alpha value, which threshhold for the number of starts must be met
    -> Double               -- ^ The Sigma Delta value, which threshhold for the metric needs to be met
    -> (a -> Graph)         -- ^ A function to create a graph from "a"
    -> (a -> [String])      -- ^ A function to summarize given the other parameters from an "a"
summarizeFrom mFn dFn n alpha delta gFn a =
        let graph = gFn a
            paths = allPathsWithSigmaAlpha alpha graph
            bests = bestPaths mFn dFn n delta paths
        in toString <$> bests