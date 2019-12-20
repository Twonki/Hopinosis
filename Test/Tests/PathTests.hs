{-# LANGUAGE OverloadedStrings #-}


module Tests.PathTests where 

import Tests.TestSuite
import Test.HUnit hiding (Node)

import qualified Data.Map.Monoidal.Strict as Map
import Data.Text hiding(map,singleton,foldr,words,null,length,head)
import Data.Monoid(Sum(..),Any(..))


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
emptyPath = []

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
    ,TestLabel "isValid_emptyPath_shouldBeFalse"  isValid_emptyPath_shouldBeFalse

    ,TestLabel "allPaths_singleSentenceGraph_shouldBeOne" allPaths_singleSentenceGraph_shouldBeOne
    ,TestLabel "allPaths_singleWordGraph_shouldBeOne" allPaths_singleWordGraph_shouldBeOne
    ,TestLabel "allPaths_emptyGraph_shouldBeZero" allPaths_emptyGraph_shouldBeZero
    ,TestLabel "allPaths_forkedSentenceGraph_shouldBeTwo" allPaths_forkedSentenceGraph_shouldBeTwo
    ,TestLabel "allPaths_twoDisjunctSentences_shouldBeTwo" allPaths_twoDisjunctSentences_shouldBeTwo
    ,TestLabel "allPaths_twoIdenticalSentences_shouldBeOne" allPaths_twoIdenticalSentences_shouldBeOne
    ,TestLabel "allPaths_twoConjoinedSentences_shouldBeFour" allPaths_twoConjoinedSentences_shouldBeFour
    ,TestLabel "allPaths_twoJoiningSentences_shouldBeTwo" allPaths_twoJoiningSentences_shouldBeTwo
    ,TestLabel "allPaths_twoForkedAndRejoinedSentences_shouldBeTwo" allPaths_twoForkedAndRejoinedSentences_shouldBeTwo

    ,TestLabel "allPaths_BugRegression1_LongSentencesShouldBe6" allPaths_BugRegression1_LongSentencesShouldBe6
    ,TestLabel "allPaths_BugRegression2_LongSentencesShouldBe9" allPaths_BugRegression2_LongSentencesShouldBe11
    ,TestLabel "allPaths_BugRegression3_LongSentencesShouldTerminate" allPaths_BugRegression3_LongSentencesShouldTerminate

    ,TestLabel "allPathsWithSigmaAlpha_SigmaAlphaIsOne_ShouldBeEmpty" allPathsWithSigmaAlpha_SigmaAlphaIsOne_ShouldBeEmpty
    ,TestLabel "allPathsWithSigmaAlpha_SigmaAlphaIsPoint99_ShouldOnlyDisplayStartsWithAlwaysStartWords" allPathsWithSigmaAlpha_SigmaAlphaIsPoint99_ShouldOnlyDisplayStartsWithAlwaysStartWords

    ,TestLabel "allPaths_EdgeCase_TwoSentences_SubPaths_shouldBeTwo" allPaths_EdgeCase_SubPaths_shouldBeTwo
    ,TestLabel "allPaths_EdgeCase_ThreeSentences_SubPaths_shouldBeThree" allPaths_EdgeCase_ThreeSentences_SubPaths_shouldBeThree

    ,TestLabel "validStartsWithSigmaAlpha_SigmaAlphaIsOne_shouldBeEmpty" validStartsWithSigmaAlpha_SigmaAlphaIsOne_shouldBeEmpty
    ,TestLabel "validStartsWithSigmaAlpha_SigmaAlphaIsZero_GraphHasOneStart_ShouldHaveOne" validStartsWithSigmaAlpha_SigmaAlphaIsZero_GraphHasOneStart_ShouldHaveOne
    ,TestLabel "validStartsWithSigmaAlpha_SigmaAlphaIsPoint49_GraphHasOneStartWithSigmaAlphaPoint5_ShouldHaveOne" validStartsWithSigmaAlpha_SigmaAlphaIsPoint49_GraphHasOneStartWithSigmaAlphaPoint5_ShouldHaveOne
    ,TestLabel "validStartsWithSigmaAlpha_SigmaAlphaIsZero_GraphHasTwoStarts_ShouldHaveTwo" validStartsWithSigmaAlpha_SigmaAlphaIsZero_GraphHasTwoStarts_ShouldHaveTwo
    ,TestLabel "validStartsWithSigmaAlpha_SigmaAlphaIsZero_GraphHasTwoStarts_ShouldHaveTwo_Type2" validStartsWithSigmaAlpha_SigmaAlphaIsZero_GraphHasTwoStarts_ShouldHaveTwo_Type2
    ,TestLabel "validStartsWithSigmaAlpha_SigmaAlphaIsPoint3_GraphHasOneFitCandidate_ShouldHaveOne" validStartsWithSigmaAlpha_SigmaAlphaIsPoint4_GraphHasOneFitCandidate_ShouldHaveOne
    ]

validStartsWithSigmaAlpha_SigmaAlphaIsOne_shouldBeEmpty = 
    0 ~=? length (Tests.TestSuite.validStartsWithSigmaAlpha 1.0 (toGraphMany ["Hello my darling test","Hello my other darling test"]))

validStartsWithSigmaAlpha_SigmaAlphaIsZero_GraphHasOneStart_ShouldHaveOne = 
    1 ~=? length (Tests.TestSuite.validStartsWithSigmaAlpha 0.0 (toGraphMany ["Hello my darling test","Hello my other darling test"]))

validStartsWithSigmaAlpha_SigmaAlphaIsPoint49_GraphHasOneStartWithSigmaAlphaPoint5_ShouldHaveOne = 
    1 ~=? length (Tests.TestSuite.validStartsWithSigmaAlpha 0.49 (toGraphMany ["Hello my darling test","Hello my other darling test"]))
    
validStartsWithSigmaAlpha_SigmaAlphaIsZero_GraphHasTwoStarts_ShouldHaveTwo = 
    2 ~=? length (Tests.TestSuite.validStartsWithSigmaAlpha 0.0 (toGraphMany ["Hello my darling test", "test what you like"]))

validStartsWithSigmaAlpha_SigmaAlphaIsZero_GraphHasTwoStarts_ShouldHaveTwo_Type2 = 
    2 ~=? length (Tests.TestSuite.validStartsWithSigmaAlpha 0.0 (toGraphMany ["Hello my darling test", "Totally different sentence"]))
    
validStartsWithSigmaAlpha_SigmaAlphaIsPoint4_GraphHasOneFitCandidate_ShouldHaveOne =
    1 ~=? length (Tests.TestSuite.validStartsWithSigmaAlpha 0.4 (toGraphMany ["Hello my darling test", "Hello my other darling test","darling should not be displayed"]))


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
isValid_emptyPath_shouldBeFalse =
    False ~=? isValid emptyPath

getStarts_ofOneDocument_shouldBeOne = 
    1 ~=? length (Tests.TestSuite.validStarts (toGraphOne "Hello"))
getStarts_ofOneWord_shouldBeWord = 
    "Hello" ~=? (fst . head . head) (Tests.TestSuite.validStarts (toGraphOne "Hello"))
getStarts_ofEmpty_shouldBeEmpty = 
    True ~=? null ( Tests.TestSuite.validStarts (toGraphOne []))
getStarts_ofTwoDocuments_shouldBeTwo = 
    2 ~=? length  (Tests.TestSuite.validStarts (toGraphMany ["Hello my darling test", "Goodbye my darling test"]))
getStarts_ofTwoDocumentsWithSameStart_shouldBeOne =
    1 ~=? length (Tests.TestSuite.validStarts (toGraphMany ["Hello my darling test", "Hello my other darling test"]))

allPaths_singleSentenceGraph_shouldBeOne = 
    1 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany ["Hello my Test"]

allPaths_singleWordGraph_shouldBeOne= 
    1 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany ["Hello"]

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

allPaths_emptyGraph_shouldBeZero = 
    0 ~=? length (allPaths Map.empty)


allPathsWithSigmaAlpha_SigmaAlphaIsOne_ShouldBeEmpty=
    0 ~=? length (Tests.TestSuite.allPathsWithSigmaAlpha 1.0 testGraph)
        where testGraph = toGraphMany ["Hello I like dogs","I like rabbits","You are different","You hate rabbits","You like me"]
{-
Collected by Hand, The "I" starting paths should be filtered. That means it's 11 -3 = 8 Paths
-}
allPathsWithSigmaAlpha_SigmaAlphaIsPoint99_ShouldOnlyDisplayStartsWithAlwaysStartWords=
    8 ~=? length (Tests.TestSuite.allPathsWithSigmaAlpha 0.99 testGraph)
        where testGraph = toGraphMany ["Hello I like dogs","I like rabbits","You are different","You hate rabbits","You like me"]
    


allPaths_BugRegression1_LongSentencesShouldBe6 = 
    6 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany ["Hello I like dogs","I like rabbits","You are different","You hate rabbits"]

allPaths_BugRegression2_LongSentencesShouldBe11 = 
    11 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany ["Hello I like dogs","I like rabbits","You are different","You hate rabbits","You like me"]

allPaths_BugRegression3_LongSentencesShouldTerminate = 
    11 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany(["Hello I like Rabbits","I like Dogs","You are Different","You hate Rabbits","You like me"])

allPaths_EdgeCase_SubPaths_shouldBeTwo =
    2 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany(["Hello I like Rabbits","I like Rabbits"])

allPaths_EdgeCase_ThreeSentences_SubPaths_shouldBeThree = 
    3 ~=? length (allPaths testGraph)
        where testGraph = toGraphMany(["Hello I like Rabbits","I like Rabbits","Rabbits"])
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
    ,TestLabel "isCyclic_emptyPath_shouldBeFalse" isCyclic_emptyPath_shouldBeFalse
    ,TestLabel "isCyclic_singletonPath_shouldBeFalse" isCyclic_singletonPath_shouldBeFalse
    ,TestLabel "InitialNextPaths_ofTwoDisjunctSentences_shouldBeTwo" nextPaths_ofTwoDisjunctSentences_shouldBeTwo
    ,TestLabel "InitialNextPaths_ofThreeDisjunctSentences_shouldBeThree" nextPaths_ofThreeDisjunctSentences_shouldBeThree
    ,TestLabel "SecondNextPath_ofTwoDiscjunctSentences_shouldBeTwo" secondNextPath_ofTwoDiscjunctSentences_shouldBeTwo2

    ,TestLabel "validStartedWithSigmaAlpha_sigmaalphaiszero_start1shouldbeTrue" validStartedWithSigmaAlpha_sigmaalphaiszero_start1shouldbeTrue
    ,TestLabel "validStartedWithSigmaAlpha_sigmaalphaiszero_start0ShouldBeFalse" validStartedWithSigmaAlpha_sigmaalphaiszero_start0ShouldBeFalse
    ,TestLabel "validStartedWithSigmaAlpha_sigmaalphaisPoint5_start2withMag3shouldbeTrue" validStartedWithSigmaAlpha_sigmaalphaisPoint5_start2withMag3shouldbeTrue
    ,TestLabel "validStartedWithSigmaAlpha_sigmaalphaisPoint5_start1withMag2shouldbeFalse" validStartedWithSigmaAlpha_sigmaalphaisPoint5_start1withMag2shouldbeFalse
    ]
isValidStarted_pathBeginsWithStart_shouldBeTrue=
    True ~=? isValidStartedWithSigmaAlpha' 0.0 validPath

isValidStarted_pathHasStartingNode_butStartingNodeIsNotStart_shouldBeFalse=
    False ~=? isValidStartedWithSigmaAlpha' 0.0 testPath
        where testPath = packNode "Test" : packStartNode "Start" : packEndNode "Other" : []

isValidStarted_pathHasNoStarts_shouldBeFalse = 
    False ~=? isValidStartedWithSigmaAlpha' 0.0 unstartedPath

isValidStarted_singletonPath_shouldBeTrue =
    True ~=? isValidStartedWithSigmaAlpha' 0.0 singletonPath

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

isCyclic_emptyPath_shouldBeFalse =
    False ~=? isCyclic []

isCyclic_singletonPath_shouldBeFalse=
    False ~=? isCyclic singletonPath



validStartedWithSigmaAlpha_sigmaalphaiszero_start1shouldbeTrue = 
    True ~=? isValidStartedWithSigmaAlpha' 0.0 testPath
        where testPath = [("SigmaTest",Values (Sum 2) Map.empty 1 (Any False))]

validStartedWithSigmaAlpha_sigmaalphaiszero_start0ShouldBeFalse = 
    False ~=? isValidStartedWithSigmaAlpha' 0.0 testPath
        where testPath = [("SigmaTest",Values (Sum 2) Map.empty 0 (Any False))]

validStartedWithSigmaAlpha_sigmaalphaisPoint5_start2withMag3shouldbeTrue = 
    True ~=? isValidStartedWithSigmaAlpha' 0.5 testPath
        where testPath = [("SigmaTest",Values (Sum 3) Map.empty (Sum 2) (Any False))]

validStartedWithSigmaAlpha_sigmaalphaisPoint5_start1withMag2shouldbeFalse = 
    False ~=? isValidStartedWithSigmaAlpha' 0.5 testPath
        where testPath = [("SigmaTest",Values (Sum 2) Map.empty 1 (Any False))]
    

nextPaths_ofTwoDisjunctSentences_shouldBeTwo = 
    2 ~=? length (nextPaths testPaths testGraph)
        where 
            testGraph = toGraphMany ["I like dogs", "You hate rabbits"]
            testPaths = Tests.TestSuite.validStarts testGraph

nextPaths_ofThreeDisjunctSentences_shouldBeThree = 
    3 ~=? length (nextPaths testPaths testGraph)
        where 
            testGraph = toGraphMany ["I like dogs", "You hate rabbits","Other Sentence"]
            testPaths = Tests.TestSuite.validStarts testGraph

secondNextPath_ofTwoDiscjunctSentences_shouldBeTwo2 =
    2 ~=? length (nextPaths firstPaths testGraph)
        where 
            testGraph = toGraphMany ["I like dogs", "You hate rabbits"]
            testPaths = Tests.TestSuite.validStarts testGraph
            firstPaths = nextPaths testPaths testGraph