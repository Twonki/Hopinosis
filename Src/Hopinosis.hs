module Hopinosis where 

import Core.Graph
import Core.Node
import Core.Path
import Core.Metric

import Data.List.Split(endByOneOf)
import qualified Data.Text as Txt

    
toGraphOne :: String -> Graph
toGraphOne s =  parseSentence $ Txt.pack <$> (words s)

toGraphMany :: [String] -> Graph
toGraphMany s = parseDocument  ((<$>) Txt.pack <$> words <$> s)

toGraphSentences:: String -> Graph 
toGraphSentences =  parseDocument . map (map Txt.pack) . map words . endByOneOf ".;:!?\n"