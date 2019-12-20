{-# LANGUAGE OverloadedStrings #-}

module Tests.NodeTests(allNodeTests,allNodeProperties) where 

import Tests.TestSuite
import Test.HUnit hiding (Node)

import Data.Monoid(Sum(..),Any(..))

import Test.Framework.Providers.QuickCheck2

import qualified Data.Map.Monoidal.Strict as Map
import Data.Text hiding(map,singleton,foldr)

allNodeTests = TestList []

allNodeProperties = [
     testProperty "v+mempty=v" prop_memptyAddition
    , testProperty "value addition" prop_valueAddition
    ]


prop_memptyAddition :: Values  -> Bool
prop_memptyAddition v = 
    v <> mempty == v

prop_valueAddition :: Values -> Values -> Bool    
prop_valueAddition v1@(Values a1 b1 c1 d1) v2@(Values a2 b2 c2 d2) =
    v1 <> v2 == Values (a1<>a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)
