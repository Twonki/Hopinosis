module Main where

import Test.HUnit

import Test.Framework
import Test.Framework.Providers.HUnit

import Tests.NodeTests
import Tests.GraphTests
import Tests.PathTests 

allTests = TestList []

tests = hUnitTestToTests allTests
main = defaultMain tests