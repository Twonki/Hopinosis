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
    uniquifie,
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
import Data.Monoid(Sum(..),Any(..))
import qualified Data.Set as Set
import Control.Monad
  ( liftM
  , liftM2
  , liftM4
  ,liftM5
  )

import Test.QuickCheck

uniValue   = Values (Sum 1) Map.empty (Sum 0) (Any False)
startValue = Values (Sum 1) Map.empty (Sum 1) (Any False) 
endValue   = Values (Sum 1) Map.empty (Sum 0) (Any True)
singletonValue = Values (Sum 1) Map.empty (Sum 1) (Any True)

    
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
    arbitrary = liftM4 Values sumOf1 arbitrary arbitrary arbitrary 

instance (Arbitrary a,Arbitrary b) => Arbitrary (Map.MonoidalMap a b) where 
    arbitrary = oneof [liftM2 Map.singleton arbitrary arbitrary, return Map.empty]
        
        --TODO: have more outs than one! 

instance Arbitrary Txt.Text where 
    arbitrary = oneof $ return . Txt.pack <$> ["A","B","C","D","E","F","G"]
    --arbitrary = Txt.pack <$> arbitrary

sumOf1 :: Gen (Sum Word)
sumOf1 = return (Sum 1)

uniquifie ::(Ord a) =>  [[a]] -> [[a]]
uniquifie = map Set.toList . Set.toList . Set.fromList . map Set.fromList
