module Hopinosis where 

import Core.Graph
import Core.Node
import Core.Path

import qualified Data.Text as Txt

hello :: Int -> String
hello = undefined


    
toGraphOne :: String -> Graph
toGraphOne s =  parseSentence $ Txt.pack <$> (words s)

toGraphMany :: [String] -> Graph
toGraphMany s = parseDocument  ((<$>) Txt.pack <$> words <$> s)

printPath = map fst
printPaths = map (map fst)
