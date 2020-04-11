{-# LANGUAGE OverloadedStrings #-}

module Tests.SelectionTests(allSelectionTests,allSelectionProperties) where 

import Tests.TestSuite
import Test.HUnit hiding (Node)
import Test.Framework.Providers.QuickCheck2

import Data.Text(Text(..))


allSelectionTests = TestList [
    TestLabel "bestPaths_Get0BestPaths_shouldBeEmpty" bestPaths_Get0BestPaths_shouldBeEmpty
    ,TestLabel "bestPaths_ofEmptyPathList_shouldBeEmpty" bestPaths_ofEmptyPathList_shouldBeEmpty
    ,TestLabel "bestPaths_ofOnePath_shouldBeOnePath" bestPaths_ofOnePath_shouldBeOnePath
    ,TestLabel "bestPaths_oneBestPaths_ofTwoDistinctPaths_shouldBeOnePath" bestPaths_oneBestPaths_ofTwoDistinctPaths_shouldBeOnePath
    ,TestLabel "bestPaths_oneBestPaths_ofThreeDistinctPaths_shouldBeOnePath" bestPaths_oneBestPaths_ofThreeDistinctPaths_shouldBeOnePath
    ,TestLabel "bestPaths_twoBestPaths_ofOnePath_shouldBeOnePath" bestPaths_twoBestPaths_ofOnePath_shouldBeOnePath
    ,TestLabel "bestPaths_threeBestPaths_ofTwoDistinctPaths_shouldBeTwoPaths" bestPaths_threeBestPaths_ofTwoDistinctPaths_shouldBeTwoPaths
    ,TestLabel "bestPaths_twoBestPaths_ofTwoDistinctPaths_shouldBeTwoPaths" bestPaths_twoBestPaths_ofTwoDistinctPaths_shouldBeTwoPaths
    ,TestLabel "bestPaths_twoBestPaths_ofTwoJoiningPaths_shouldBeTwoPaths" bestPaths_twoBestPaths_ofTwoJoiningPaths_shouldBeTwoPaths
    ,TestLabel "bestPaths_twoBestPaths_ofTwoForkingPaths_shouldBeTwoPaths" bestPaths_twoBestPaths_ofTwoForkingPaths_shouldBeTwoPaths
    ]

allSelectionProperties = [

    ]

       
bestPaths_Get0BestPaths_shouldBeEmpty =
    0 ~=? length (anyBestPathsWithStart 0 testPaths )
        where
            testPath = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPaths = [testPath]

bestPaths_ofEmptyPathList_shouldBeEmpty = 
    0 ~=? length (anyBestPathsWithStart 1 testPaths )
        where
            testPaths = []

bestPaths_ofOnePath_shouldBeOnePath = 
    1 ~=? length (anyBestPathsWithStart 1 testPaths)
        where
            testPath = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPaths = [testPath]

bestPaths_oneBestPaths_ofTwoDistinctPaths_shouldBeOnePath=
    1 ~=? length (anyBestPathsWithStart 1 testPaths)
        where
            testPath = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPath2 = packStartNode "Bye" : packNode "other" : packNode "any" :packEndNode "Node" : []
            testPaths = [testPath,testPath2]


bestPaths_oneBestPaths_ofThreeDistinctPaths_shouldBeOnePath=
    1 ~=? length (anyBestPathsWithStart 1 testPaths)
        where
            testPath = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPath2 = packStartNode "Bye" : packNode "other" : packNode "any" :packEndNode "Node" : []
            testPath3 = packStartNode "Third" : packEndNode "Path" : []
            testPaths = [testPath,testPath2, testPath3]

bestPaths_twoBestPaths_ofOnePath_shouldBeOnePath=
    1 ~=? length (anyBestPathsWithStart 2 testPaths)
        where
            testPath = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPaths = [testPath]

bestPaths_twoBestPaths_ofTwoDistinctPaths_shouldBeTwoPaths=
    2 ~=? length (anyBestPathsWithStart 2 testPaths)
        where
            testPath = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPath2 = packStartNode "Bye" : packNode "other" : packNode "any" :packEndNode "Node" : []
            testPaths = [testPath,testPath2]

bestPaths_threeBestPaths_ofTwoDistinctPaths_shouldBeTwoPaths=
    2 ~=? length (anyBestPathsWithStart 3 testPaths)
        where
            testPath = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPath2 = packStartNode "Bye" : packNode "other" : packNode "any" :packEndNode "Node" : []
            testPaths = [testPath,testPath2]
            


bestPaths_twoBestPaths_ofTwoJoiningPaths_shouldBeTwoPaths=
    2 ~=? length (anyBestPathsWithStart 2 testPaths)
        where
            testPath = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPath2 = packStartNode "Bye" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPaths = [testPath,testPath2]
            

bestPaths_twoBestPaths_ofTwoForkingPaths_shouldBeTwoPaths=
    2 ~=? length (anyBestPathsWithStart 2 testPaths)
        where
            testPath = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPath2 = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Friends" : []
            testPaths = [testPath,testPath2]

-- Small helper with most simplest Distance and Metric, but needs a start 
anyBestPathsWithStart n = bestPaths (\x->1) (\a b->1) n 0.025