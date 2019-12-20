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
    packStartNode,
    -- Export my QuickCheck thingies
    arbitrary
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
import Control.Monad
  ( liftM
  , liftM2
  , liftM4
  ,liftM5
  )

import Test.QuickCheck

uniValue   = Values 1 Map.empty 0 False
startValue = Values 1 Map.empty 1 False 
endValue   = Values 1 Map.empty 0 True
singletonValue = Values 1 Map.empty 1 True

    
toGraphOne :: String -> Graph
toGraphOne s =  parseSentence $ Txt.pack <$> words s

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

instance Arbitrary Values where 
    arbitrary = liftM4 Values arbitrary arbitrary arbitrary arbitrary 

instance (Arbitrary a,Arbitrary b) => Arbitrary (Map.MonoidalMap a b) where 
    arbitrary = oneof [liftM2 Map.singleton arbitrary arbitrary, return Map.empty]
        
        --return Map.empty --TODO: This is quite minimalistic

instance Arbitrary Txt.Text where 
    arbitrary = Txt.pack <$> arbitrary
