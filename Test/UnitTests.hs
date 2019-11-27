module Main where

import Test.HUnit

import Test.Framework
import Test.Framework.Providers.HUnit

import Tests.NodeTests
import Tests.GraphTests
import Tests.PathTests 
import Tests.MetricTests

allTests = 
    TestList [
        allNodeTests,
        allGraphTests,
        allPathTests,
        allMetricTests
    ]

tests = hUnitTestToTests allTests
main = defaultMain tests