{-# LANGUAGE OverloadedStrings #-}

module Tests.SelectionTests(allSelectionTests) where 

import Tests.TestSuite
import Test.HUnit hiding (Node)

import Data.Text(Text(..))

allSelectionTests = TestList [
    TestLabel "bestPaths_Get0BestPaths_shouldBeEmpty" bestPaths_Get0BestPaths_shouldBeEmpty
    ,TestLabel "commonBestPaths_ofEmptyPathList_shouldBeEmpty" commonBestPaths_ofEmptyPathList_shouldBeEmpty
    --,TestLabel "commonBestPaths_ofOnePath_shouldBeOnePath" commonBestPaths_ofOnePath_shouldBeOnePath
    ]

bestPaths_Get0BestPaths_shouldBeEmpty =
    0 ~=? length (bestPaths (\x->1) (\a b->1) 0 0.25 testPaths )
        where
            testPath = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPaths = [testPath]

commonBestPaths_ofEmptyPathList_shouldBeEmpty = 
    0 ~=? length (commonBestPaths testPaths )
        where
            testPaths = []

commonBestPaths_ofOnePath_shouldBeOnePath = 
    1 ~=? length (commonBestPaths testPaths)
        where
            testPath = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPaths = [testPath]