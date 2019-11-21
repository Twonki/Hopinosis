module Tests.TestSuite(
    module Core.Graph,
    module Core.Node,
    module Core.Path,
    toGraphOne,
    toGraphMany,
    uniValue,
    startValue,
    endValue
    )
where 
import Core.Graph
import Core.Node
import Core.Path
import qualified Data.Map.Monoidal.Strict as Map
import qualified Data.Text as Txt


uniValue   = Values 1 Map.empty False False
startValue = Values 1 Map.empty True False 
endValue   = Values 1 Map.empty False True


    
toGraphOne :: String -> Graph
toGraphOne s =  parseSentence (map Txt.pack (words s))

toGraphMany :: [String] -> Graph
toGraphMany s = parseDocument  (map (map Txt.pack) (map words s))