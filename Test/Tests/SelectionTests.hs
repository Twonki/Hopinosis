{-# LANGUAGE OverloadedStrings #-}

module Tests.SelectionTests(allSelectionTests,allSelectionProperties) where 

import Tests.TestSuite
import Test.HUnit hiding (Node)
import Test.Framework.Providers.QuickCheck2

import Data.Text(Text(..))


import qualified Core.Selection.Internals as Intern

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
    testProperty "Cannot make n-tuples from to short candidates" prop_listTooShort_noTuplesMade
    , testProperty "Tuples have length n" prop_makeTuples_tuplesHaveLengthN
    , testProperty "Tuples are Unique" prop_tuplesAreUnique
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

{- 
These properties are usually having the signature (Ord a) => [a] -> Word -> Bool
But then you cannot properly compile them, as they have an ambigious type reference. 
(You can try those out with just changing the signature, then you get the same error)

I have chosen Text as hardcoded type, as it is the most important for my use-case. 
-}

prop_listTooShort_noTuplesMade :: [Text] -> Word -> Bool
prop_listTooShort_noTuplesMade xs n = 
    if length xs < fromIntegral n 
    then length (Intern.ntuples xs n) == 0
    else True -- actual property of qualified tuples is done separate

prop_makeTuples_tuplesHaveLengthN ::[Text] -> Word -> Bool
prop_makeTuples_tuplesHaveLengthN xs n = 
    if length xs >= fromIntegral n 
    then all (\x -> length x == (fromIntegral n)) (Intern.ntuples xs n)
    else True 


prop_tuplesAreUnique ::[Text] -> Word -> Bool
prop_tuplesAreUnique xs n = 
    let tuples = Intern.ntuples xs n 
    in length tuples == length (uniquifie tuples)
            


-- Small helper with most simplest Distance and Metric, but needs a start 
anyBestPathsWithStart n = bestPaths (\x->1) (\a b->1) n 0.025