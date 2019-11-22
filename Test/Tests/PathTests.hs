{-# LANGUAGE OverloadedStrings #-}


module Tests.PathTests where 

import Tests.TestSuite
import Test.HUnit hiding (Node)

import qualified Data.Map.Monoidal.Strict as Map
import Data.Text hiding(map,singleton,foldr,words,null,length,head)
    
import Tests.TestSuite

import Core.Path.Internals hiding (starts,isValid,allPaths)

allPathTests = TestList [
    TestLabel "exposed PathFunctions" pathTests
    , TestLabel "Internal PathFunctions" pathInternalTests
    ]

{-
    Example Paths
-}

validPath = packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
singletonPath = packSingletonNode "Alone":[]
cyclicPath = packStartNode "Hello" : packNode "you" : packNode "Hello" :packEndNode "World" : []
unstartedPath = packNode "to" : packNode "my" :packEndNode "Test" : []
unendedPath = packStartNode "Hello" : packNode "to" : packNode "my" : []

{-
    Test for Exposed Functions
-}
pathTests = TestList [
    TestLabel "getStarts_ofOneDocument_shouldBeOne" getStarts_ofOneDocument_shouldBeOne
    ,TestLabel "getStarts_ofOneWord_shouldBeWord" getStarts_ofOneWord_shouldBeWord
    ,TestLabel "getStarts_ofEmpty_shouldBeEmpty" getStarts_ofEmpty_shouldBeEmpty
    ,TestLabel "getStarts_ofTwoDocuments_shouldBeTwo" getStarts_ofTwoDocuments_shouldBeTwo
    ,TestLabel "getStarts_ofTwoDocumentsWithSameStart_shouldBeOne" getStarts_ofTwoDocumentsWithSameStart_shouldBeOne

    ,TestLabel "isValid_validPath_shouldBeTrue" isValid_validPath_shouldBeTrue
    ,TestLabel "isValid_cyclicPath_shouldBeFalse" isValid_cyclicPath_shouldBeFalse
    ,TestLabel "isValid_unstartedPath_shouldBeFalse" isValid_unstartedPath_shouldBeFalse
    ,TestLabel "isValid_unendedPath_shouldBeFalse" isValid_unendedPath_shouldBeFalse
    ,TestLabel "isValid_singletonPath_shouldBeTrue" isValid_singletonPath_shouldBeTrue

    ,TestLabel "allPaths_singleSentenceGraph_shouldBeOne" allPaths_singleSentenceGraph_shouldBeOne
    ,TestLabel "allPaths_forkedSentenceGraph_shouldBeTwo" allPaths_forkedSentenceGraph_shouldBeTwo
    ,TestLabel "allPaths_twoDisjunctSentences_shouldBeTwo" allPaths_twoDisjunctSentences_shouldBeTwo
    ,TestLabel "allPaths_twoIdenticalSentences_shouldBeOne" allPaths_twoIdenticalSentences_shouldBeOne
    ,TestLabel "allPaths_twoConjoinedSentences_shouldBeFour" allPaths_twoConjoinedSentences_shouldBeFour
    ,TestLabel "allPaths_twoJoiningSentences_shouldBeTwo" allPaths_twoJoiningSentences_shouldBeTwo
    ,TestLabel "allPaths_twoForkedAndRejoinedSentences_shouldBeTwo" allPaths_twoForkedAndRejoinedSentences_shouldBeTwo
    ]

isValid_validPath_shouldBeTrue =
    True ~=? isValid validPath
isValid_cyclicPath_shouldBeFalse =
    False ~=? isValid cyclicPath
isValid_unstartedPath_shouldBeFalse =
    False ~=? isValid unstartedPath
isValid_unendedPath_shouldBeFalse =
    False ~=? isValid unendedPath
isValid_singletonPath_shouldBeTrue =
    True ~=? isValid singletonPath

getStarts_ofOneDocument_shouldBeOne = 
    1 ~=? length (starts (toGraphOne "Hello"))
getStarts_ofOneWord_shouldBeWord = 
    "Hello" ~=? (fst . head) (starts (toGraphOne "Hello"))
getStarts_ofEmpty_shouldBeEmpty = 
    True ~=? null ( starts (toGraphOne []))
getStarts_ofTwoDocuments_shouldBeTwo = 
    2 ~=? length (starts (toGraphMany ["Hello my darling test", "Goodbye my darling test"]))
getStarts_ofTwoDocumentsWithSameStart_shouldBeOne =
    1 ~=? length (starts (toGraphMany ["Hello my darling test", "Hello my other darling test"]))

allPaths_singleSentenceGraph_shouldBeOne = 
    1 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany ["Hello my Test"]

allPaths_forkedSentenceGraph_shouldBeTwo = 
    2 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany ["Hello my Test","Hello my Love"]

allPaths_twoDisjunctSentences_shouldBeTwo= 
    2 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany ["Hello my Test","I like Haskell"]

allPaths_twoIdenticalSentences_shouldBeOne = 
    1 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany ["Hello my Test","Hello my Test"]
 
allPaths_twoConjoinedSentences_shouldBeFour = 
    4 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany ["Hello to Test","Hi to Haskell"]     

allPaths_twoJoiningSentences_shouldBeTwo= 
    2 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany ["Hello my Test","Hi my Test"]

allPaths_twoForkedAndRejoinedSentences_shouldBeTwo= 
    2 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany ["Hello my Test","Hello your Test"]

{-
    Test for Internal Functions
-}

pathInternalTests = TestList [
    TestLabel "isValidStarted_pathBeginsWithStart_shouldBeTrue"  isValidStarted_pathBeginsWithStart_shouldBeTrue
    ,TestLabel "isValidStarted_pathHasStartingNode_butStartingNodeIsNotStart_shouldBeFalse" isValidStarted_pathHasStartingNode_butStartingNodeIsNotStart_shouldBeFalse
    ,TestLabel "isValidStarted_pathHasNoStarts_shouldBeFalse" isValidStarted_pathHasNoStarts_shouldBeFalse
    ,TestLabel "isValidStarted_singletonPath_shouldBeTrue" isValidStarted_singletonPath_shouldBeTrue
    ,TestLabel "isValidEnded_pathEndsWithEnd_shouldBeTrue" isValidEnded_pathEndsWithEnd_shouldBeTrue
    ,TestLabel "isValidEnded_pathHasEndingNode_butEndingNodeIsNotEnd_shouldBeFalse" isValidEnded_pathHasEndingNode_butEndingNodeIsNotEnd_shouldBeFalse
    ,TestLabel "isValidEnded_pathHasNoEnds_shouldBeFalse" isValidEnded_pathHasNoEnds_shouldBeFalse
    ,TestLabel "isValidEnded_singletonPath_shouldBeTrue" isValidEnded_singletonPath_shouldBeTrue
    ,TestLabel "isCyclic_nonCyclicPath_shouldBeFalse" isCyclic_nonCyclicPath_shouldBeFalse
    ,TestLabel "isCyclic_cyclicPath_shouldBeTrue" isCyclic_cyclicPath_shouldBeTrue
    ,TestLabel "isCyclic_singletonPath_shouldBeFalse" isCyclic_singletonPath_shouldBeFalse
    ]
isValidStarted_pathBeginsWithStart_shouldBeTrue=
    True ~=? isValidStarted validPath

isValidStarted_pathHasStartingNode_butStartingNodeIsNotStart_shouldBeFalse=
    False ~=? isValidStarted testPath
        where testPath = packNode "Test" : packStartNode "Start" : packEndNode "Other" : []

isValidStarted_pathHasNoStarts_shouldBeFalse = 
    False ~=? isValidStarted unstartedPath

isValidStarted_singletonPath_shouldBeTrue =
    True ~=? isValidStarted singletonPath
    

isValidEnded_pathEndsWithEnd_shouldBeTrue=
    True ~=? isValidEnded validPath

isValidEnded_pathHasEndingNode_butEndingNodeIsNotEnd_shouldBeFalse =
    False ~=? isValidEnded testPath
        where testPath = packNode "Test" : packEndNode "Other" : packStartNode "Start" : []

isValidEnded_pathHasNoEnds_shouldBeFalse =
    False ~=? isValidEnded unendedPath

isValidEnded_singletonPath_shouldBeTrue =
    True ~=? isValidEnded singletonPath
    

isCyclic_nonCyclicPath_shouldBeFalse = 
    False ~=? isCyclic validPath

isCyclic_cyclicPath_shouldBeTrue =
    True ~=? isCyclic cyclicPath 

isCyclic_singletonPath_shouldBeFalse=
    False ~=? isCyclic singletonPath