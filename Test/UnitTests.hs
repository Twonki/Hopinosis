module Main where

import Test.HUnit

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Test.QuickCheck

import Tests.NodeTests
import Tests.GraphTests
import Tests.PathTests 
import Tests.MetricTests
import Tests.SelectionTests


allTests = 
    TestList [
        allNodeTests,
        allGraphTests,
        allPathTests,
        allMetricTests,
        allSelectionTests
    ]

tests = hUnitTestToTests allTests ++ 
        (uncurry testProperty <$> nodeQuickCheckProps)
main = 
    defaultMain tests
     