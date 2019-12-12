{-# LANGUAGE OverloadedStrings #-}

module Tests.TestSuite(
    module Core.Types,
    module Core.Selection,
    module Core.Graph,
    module Core.Node,
    module Core.Path,
    module Core.Metric,
    toGraphOne,
    toGraphMany,
    uniValue,
    startValue,
    singletonValue,
    endValue,
    packNode,
    packSingletonNode,
    packEndNode,
    packStartNode
    )
where 
import Core.Types
import Core.Graph
import Core.Node
import Core.Path
import Core.Metric
import Core.Selection
import qualified Data.Map.Monoidal.Strict as Map
import qualified Data.Text as Txt



uniValue   = Values 1 Map.empty 0 False
startValue = Values 1 Map.empty 1 False 
endValue   = Values 1 Map.empty 0 True
singletonValue = Values 1 Map.empty 1 True

    
toGraphOne :: String -> Graph
toGraphOne s =  parseSentence $ Txt.pack <$> (words s)

toGraphMany :: [String] -> Graph
toGraphMany s = parseDocument  ((<$>) Txt.pack <$> words <$> s)


packNode :: Txt.Text -> Node 
packNode s = (s,uniValue)

packSingletonNode :: Txt.Text -> Node 
packSingletonNode s = (s,singletonValue)

packEndNode :: Txt.Text -> Node 
packEndNode s = (s,endValue)

packStartNode :: Txt.Text -> Node 
packStartNode s = (s,startValue)