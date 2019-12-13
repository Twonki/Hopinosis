{-|
Module      : Core.Graph
Description : Parsing sentences into an opinosis graph
License     : MIT
Maintainer  : Leonhard.Applis@Protonmail.com
-}
module Core.Graph where 

import Core.Node
import Core.Types

import Data.Text(Text(..)) 
import qualified Data.Map.Monoidal.Strict as Map

-- | Builds a graph from a list of nodes. 
-- 
-- This can be used to build graphs from paths - however the graphs will maybe have dead-end edges. 
fromNodes :: [Node] -> Graph
fromNodes nds = mconcat $ map (\(k,v) -> Map.singleton k v) nds


-- |Takes a list of list of words and produces an graph. 
-- 
-- The words have to be in correct order of their occurrence, including duplicates. 
parseDocument:: [[Text]] -> Graph
parseDocument = mconcat . map parseSentence

-- |Takes a list of words and produces an graph. 
-- 
-- The words have to be in correct order of their occurrence, including duplicates. 
parseSentence:: [Text] -> Graph
parseSentence s = tagStart s $ parse s
    where 
        parse :: [Text] -> Graph
        parse [] = mempty
        parse (w:[]) = Map.singleton w (Values 1 Map.empty 0 True)
        parse (w:o:ws) = 
                        let n = Map.singleton w (Values 1 (Map.singleton o 1) 0 False)
                        in  n <> parse (o:ws)
        tagStart :: [Text] -> Graph -> Graph
        tagStart [] g = g
        tagStart (s:_) g =  Map.adjust setStart s g