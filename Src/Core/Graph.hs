module Core.Graph where 

import Core.Node
import Data.List

type Graph = [Node]

parseDocument:: [[String]] -> Graph
parseDocument = (foldr mergeGraphs []) . map parseSentence'

parseSentence':: [String] -> Graph
parseSentence' s = let g = parseSentence s
                   in tagStart g s

parseSentence:: [String] -> Graph
parseSentence (w:[]) = (w,Values 1 [] False True) :[]
parseSentence (w:o:ws) = (w, Values 1 [(o,1)] False False) : parseSentence (o:ws) 

tagStart:: Graph -> [String] -> Graph
tagStart g (s:ws) = 
    let t:ts = filter (\(k,v) -> k==s ) g
    in setStart t : ts

mergeGraphs :: Graph -> Graph -> Graph
mergeGraphs g1 g2 = foldGraph (g1 ++ g2)

foldGraph:: Graph -> Graph 
foldGraph g = 
    let 
        -- Group all Nodes by their key 
        gs = groupBy ( \ (k,v) (k2,v2) -> k ==k2 ) g
        -- Make [[(x,v)],...] to [(x,[v]),...]
        ks = map foldHelp gs
        -- Apply [v]->v to (x,[v])
        ks' = map (\(k,vs) -> (k,foldValues vs)) ks
        -- Return [(x,v)]
    in ks'

foldHelp :: [(String, Values)] -> (String,[Values])
foldHelp kvs@((k,_):_) = let key = k 
                             values =  map (\ (k,v) -> v) kvs 
                         in (k,values)
