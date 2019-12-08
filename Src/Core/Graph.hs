module Core.Graph where 

import Core.Node
import Core.Types

import Data.Text(Text(..)) 
import qualified Data.Map.Monoidal.Strict as Map

fromNodes :: [Node] -> Graph
fromNodes nds = mconcat $ map (\(k,v) -> Map.singleton k v) nds

parseDocument:: [[Text]] -> Graph
parseDocument = mconcat . map parseSentence

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