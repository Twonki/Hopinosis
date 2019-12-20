module Main where

import Test.HUnit

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

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

allProperties = allNodeProperties -- ++ allPathProperties ++ allMetricProperties ... 

tests = hUnitTestToTests allTests ++ allProperties
main = 
    defaultMain tests
     