module Core.Graph where 

import Core.Node
import Data.List

import qualified Data.Map as Map


type Graph = Map.Map String Values

parseDocument:: [[String]] -> Graph
parseDocument = (foldr merge Map.empty) . map parseSentence'

parseSentence':: [String] -> Graph
parseSentence' s = let g = parseSentence s
                   in tagStart s g

parseSentence :: [String] -> Graph
parseSentence (w:[]) = Map.singleton w (Values 1 Map.empty False True)
parseSentence (w:o:ws) = 
                let n = Map.singleton w (Values 1 (Map.singleton o 1) False False)
                in Map.unionWith (<>) n (parseSentence (o:ws))

tagStart :: [String] -> Graph -> Graph
tagStart (s:_) = Map.adjust setStart s 

merge :: Graph -> Graph -> Graph
merge = Map.unionWith (<>)
