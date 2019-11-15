module Core.Graph where 

import Core.Node
import qualified Data.Map as Map
import Data.Text hiding (map,singleton)

newtype Graph = G (Map.Map Text Values)
    deriving (Eq, Show)

instance Semigroup Graph where 
    (<>) (G a) (G b) = G $ Map.unionWith (<>) a b 
instance Monoid Graph where
    mempty = G Map.empty

singleton :: Text -> Values -> Graph
singleton s v= G $ Map.singleton s v

fromNodes :: [Node] -> Graph
fromNodes nds = mconcat $ map (\(k,v) -> singleton k v) nds
 
parseDocument:: [[Text]] -> Graph
parseDocument = mconcat . map parseSentence

parseSentence:: [Text] -> Graph
parseSentence s = tagStart s $ parse s
    where 
        parse :: [Text] -> Graph
        parse [] = mempty
        parse (w:[]) = singleton w (Values 1 Map.empty False True)
        parse (w:o:ws) = 
                        let n = singleton w (Values 1 (Map.singleton o 1) False False)
                        in  n <> (parse (o:ws))
        tagStart :: [Text] -> Graph -> Graph
        tagStart (s:_) =  lift $ Map.adjust setStart s
        
lift :: ((Map.Map Text Values) -> (Map.Map Text Values)) -> Graph -> Graph
lift f (G u) = G $ f u

unwrap:: Graph -> Map.Map Text  Values
unwrap (G u) = u 