module Core.Graph where 

import Core.Node

type Graph = [Node]

parseSentence:: [String] -> Graph
parseSentence (w:[]) = (w,Values 1 [] False True) :[]
parseSentence (w:o:ws) = (w, Values 1 [(o,1)] False False) : parseSentence (o:ws) 

tagStart:: Graph -> [String] -> Graph
tagStart g (s:ws) = 
    let t:ts = filter (\(k,v) -> k==s ) g
    in setStart t : ts