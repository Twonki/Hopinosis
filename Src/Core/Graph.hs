module Core.Graph where 

import Core.Node

import qualified Data.Map as Map

type Graph = Map.Map String Values

newtype Graph2 = G (Map.Map String Values)
    deriving (Eq, Show)

emptyG :: Graph2 
emptyG = G Map.empty

mappendG :: Graph2 -> Graph2 -> Graph2
mappendG (G a) (G b) = G $ Map.unionWith (<>) a b 

singleton :: String -> Values -> Graph2
singleton s v= G $ Map.singleton s v
 
parseSentence2 :: [String] -> Graph2
parseSentence2 (w:[]) = singleton w (Values 1 Map.empty False True)
parseSentence2 (w:o:ws) = 
                let n = singleton w (Values 1 (Map.singleton o 1) False False)
                in mappendG n (parseSentence2 (o:ws))

tagStart2 :: [String] -> Graph2 -> Graph2
tagStart2 (s:_) g =  G $ Map.adjust setStart s (unwrap g)
                
unwrap:: Graph2 -> Map.Map String  Values
unwrap (G u) = u 


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
