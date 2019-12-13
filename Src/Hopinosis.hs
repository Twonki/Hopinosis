{-|
Module      : Hopinosis
Description : Toplevel Functions for the Opinosis Algorithm
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com

This Module contains several functions to make and use the Opinosis summaries from strings.
-}
module Hopinosis where 

import Core.Graph
import Core.Path
import Core.Metric
import Core.Selection
import Core.Types

import Data.List.Split(endByOneOf)
import qualified Data.Text as Txt

-- |parses one sentence to an opinosis graph.    
toGraphOne :: String -> Graph
toGraphOne s =  parseSentence $ Txt.pack <$> (words s)

-- |parses a list of sentences to an opinosis graph.
toGraphMany :: [String] -> Graph
toGraphMany s = parseDocument  ((<$>) Txt.pack <$> words <$> s)

-- |parses an un-split multisentence-text to an opinosis graph.
toGraphSentences:: String -> Graph 
toGraphSentences =  parseDocument . map (map Txt.toLower) . map (map Txt.pack) . map words . endByOneOf ".;:!?\n"

-- |forms a readable sentence from a path.
toString :: Path -> String
toString p = Txt.unpack (pathJoin (fst <$> p))
    where
        pathJoin = Txt.intercalate (Txt.pack " ")

-- | Takes an unsplit multisentence-text, parses it into a graph, finds the best 3 paths for common parameters and returns the paths readable. 
-- 
-- This is the primary function of the module.
opinosisSummary :: String -> [String]
opinosisSummary s = 
    let graph = toGraphSentences s
        paths = allPaths graph
        bests = commonBestPaths paths
    in toString <$> bests