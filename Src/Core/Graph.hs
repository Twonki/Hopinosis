module Core.Graph where 

import Core.Node
import qualified Data.Map as Map
import Data.Text hiding (map,singleton)

type Graph = Map.Map Text Values

fromNodes :: [Node] -> Graph
fromNodes nds = mconcat $ map (\(k,v) -> Map.singleton k v) nds
 
parseDocument:: [[Text]] -> Graph
parseDocument = mconcat . map parseSentence

parseSentence:: [Text] -> Graph
parseSentence s = tagStart s $ parse s
    where 
        parse :: [Text] -> Graph
        parse [] = mempty
        parse (w:[]) = Map.singleton w (Values 1 Map.empty False True)
        parse (w:o:ws) = 
                        let n = Map.singleton w (Values 1 (Map.singleton o 1) False False)
                        in  Map.unionWith (<>) n (parse (o:ws))
        tagStart :: [Text] -> Graph -> Graph
        tagStart [] g = g
        tagStart (s:_) g =  Map.adjust setStart s g