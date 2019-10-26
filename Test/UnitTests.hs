module Main where

import Test.HUnit

import Test.Framework
import Test.Framework.Providers.HUnit

import Tests.PawnTests
import Tests.CheckTests
import Tests.GameTests
import Tests.MovementTests
import Tests.CoreMovementTests

allTests = TestList []

tests = hUnitTestToTests allTests
main = defaultMain tests