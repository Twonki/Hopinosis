module Hopinosis where 

import Core.Graph
import Core.Path
import Core.Metric
import Core.Selection
import Core.Types

import Data.List.Split(endByOneOf)
import qualified Data.Text as Txt

    
toGraphOne :: String -> Graph
toGraphOne s =  parseSentence $ Txt.pack <$> (words s)

toGraphMany :: [String] -> Graph
toGraphMany s = parseDocument  ((<$>) Txt.pack <$> words <$> s)

toGraphSentences:: String -> Graph 
toGraphSentences =  parseDocument . map (map Txt.toLower) . map (map Txt.pack) . map words . endByOneOf ".;:!?\n"

toString :: Path -> String
toString p = Txt.unpack (pathJoin (fst <$> p))
    where
        pathJoin = Txt.intercalate (Txt.pack " ")

opinosisSummary :: String -> [String]
opinosisSummary s = 
    let graph = toGraphSentences s
        paths = allPaths graph
        bests = commonBestPaths paths
    in toString <$> bests