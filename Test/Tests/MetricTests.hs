{-# LANGUAGE OverloadedStrings #-}

module Tests.MetricTests(allMetricTests) where 

import Tests.TestSuite
import Test.HUnit hiding (Node)

import Data.Text(Text(..))

allMetricTests = TestList [
    TestLabel "magnitudes_singleSentenceGraph_shouldBeSentenceLength" magnitudes_singleSentenceGraph_shouldBeSentenceLength
    ,TestLabel "averagedMagnitudes_singleSentenceGraph_shouldBeOne" averagedMagnitudes_singleSentenceGraph_shouldBeOne
    ,TestLabel "edgeStrength_singleSentenceGraph_shouldBeSentenceLengthMinusOne" edgeStrength_singleSentenceGraph_shouldBeSentenceLengthMinusOne
    ,TestLabel "averagedEdgeStrength_singleSentenceGraph_shouldBeSmallerOne" averagedEdgeStrength_singleSentenceGraph_shouldBeSmallerOne

    ,TestLabel "magnitudes_DoubleSentenceGraph_shouldBeTwiceSentenceLength" magnitudes_DoubleSentenceGraph_shouldBeTwiceSentenceLength
    ,TestLabel "averagedMagnitudes_DoubleSentenceGraph_shouldBeTwo" averagedMagnitudes_DoubleSentenceGraph_shouldBeTwo
    ,TestLabel "edgeStrength_DoubleSentenceGraph_shouldBeTwoTimesSentenceLengthMinusTwo" edgeStrength_DoubleSentenceGraph_shouldBeTwoTimesSentenceLengthMinusTwo
    ,TestLabel "averagedEdgeStrength_DoubleSentenceGraph_shouldBeBiggerThanOneAndSmallerThanTwo" averagedEdgeStrength_DoubleSentenceGraph_shouldBeBiggerThanOneAndSmallerThanTwo

    ,TestLabel "toVectors_shouldHaveSameSize" toVectors_shouldHaveSameSize
    ,TestLabel "toVectors_onePathEmpty_shouldHaveSameSize" toVectors_onePathEmpty_shouldHaveSameSize
    ,TestLabel "toVectors_secondPathEmpty_shouldHaveSameSize" toVectors_secondPathEmpty_shouldHaveSameSize
    ,TestLabel "toVectors_6distinctWords_shouldHaveLength6" toVectors_6distinctWords_shouldHaveLength6
    ,TestLabel "toVectors_6distinctWords_otherVector_shouldHaveLength6Too" toVectors_6distinctWords_otherVector_shouldHaveLength6Too
    ,TestLabel "toVectors_onePathEmpty_otherHas3Words_shouldHaveLength3" toVectors_onePathEmpty_otherHas3Words_shouldHaveLength3
    ,TestLabel "toVectors_onePathEmpty_otherVector_otherHas3Words_shouldHaveLength3too" toVectors_onePathEmpty_otherVector_otherHas3Words_shouldHaveLength3too

    ,TestLabel "cosineSim_samePaths_shouldBeOne" cosineSim_samePaths_shouldBeOne
    ,TestLabel "cosineSim_CompleteDifferentPaths_shouldBeZero" cosineSim_CompleteDifferentPaths_shouldBeZero
    ,TestLabel "cosineSim_OneEmptyPath_shouldBeZero" cosineSim_OneEmptyPath_shouldBeZero
    ,TestLabel "cosineSim_TwoEmptyPath_shouldBeZero" cosineSim_TwoEmptyPath_shouldBeZero
    ,TestLabel "cosineSim_FirstPathEmpty_shouldBeZero" cosineSim_FirstPathEmpty_shouldBeZero

    ,TestLabel "jaccardSim_samePaths_shouldBeOne" jaccardSim_samePaths_shouldBeOne
    ,TestLabel "jaccardSim_CompleteDifferentPaths_shouldBeZero" jaccardSim_CompleteDifferentPaths_shouldBeZero
    ,TestLabel "jaccardSim_secondPathEmpty_shouldBeZero" jaccardSim_secondPathEmpty_shouldBeZero
    ,TestLabel "jaccardSim_firstPathEmpty_shouldBeZero" jaccardSim_firstPathEmpty_shouldBeZero
    ,TestLabel "jaccardSim_bothPathsEmpty_shouldBeZero" jaccardSim_bothPathsEmpty_shouldBeZero
    ,TestLabel "jaccardSim_oneOfTwoWordsOverlapping_shouldBePoint5" jaccardSim_oneOfTwoWordsOverlapping_shouldBePoint5
    ,TestLabel "jaccardSim_oneOfTwoWordsOverlapping_shouldBeSymmetric" jaccardSim_oneOfTwoWordsOverlapping_shouldBeSymmetric
    ]

magnitudes_singleSentenceGraph_shouldBeSentenceLength = 
    4 ~=? magnitudes testPath 
        where 
            testGraph = toGraphOne "Hello I am Test"
            testPaths = allPaths testGraph 
            testPath = head testPaths

averagedMagnitudes_singleSentenceGraph_shouldBeOne =
    1 ~=? averagedMagnitudes testPath 
        where 
            testGraph = toGraphOne "Hello I am Test"
            testPaths = allPaths testGraph 
            testPath = head testPaths


edgeStrength_singleSentenceGraph_shouldBeSentenceLengthMinusOne = 
    3 ~=? edgeStrengths testPath 
        where 
            testGraph = toGraphOne "Hello I am Test"
            testPaths = allPaths testGraph 
            testPath = head testPaths

averagedEdgeStrength_singleSentenceGraph_shouldBeSmallerOne =
    True ~=? 1> averagedEdgeStrengths testPath && 0 <  averagedEdgeStrengths testPath
        where 
            testGraph = toGraphOne "Hello I am Test"
            testPaths = allPaths testGraph 
            testPath = head testPaths

magnitudes_DoubleSentenceGraph_shouldBeTwiceSentenceLength = 
    8 ~=? magnitudes testPath 
        where 
            testGraph = toGraphMany ["Hello I am Test","Hello I am Test"]
            testPaths = allPaths testGraph 
            testPath = head testPaths

averagedMagnitudes_DoubleSentenceGraph_shouldBeTwo = 
    2 ~=? averagedMagnitudes testPath 
        where 
            testGraph = toGraphMany ["Hello I am Test","Hello I am Test"]
            testPaths = allPaths testGraph 
            testPath = head testPaths

edgeStrength_DoubleSentenceGraph_shouldBeTwoTimesSentenceLengthMinusTwo = 
    6 ~=? edgeStrengths testPath 
        where 
            testGraph = toGraphMany ["Hello I am Test","Hello I am Test"]
            testPaths = allPaths testGraph 
            testPath = head testPaths

averagedEdgeStrength_DoubleSentenceGraph_shouldBeBiggerThanOneAndSmallerThanTwo = 
    True ~=? 2> averagedEdgeStrengths testPath && 1 <  averagedEdgeStrengths testPath 
        where 
            testGraph = toGraphMany ["Hello I am Test","Hello I am Test"]
            testPaths = allPaths testGraph 
            testPath = head testPaths


toVectors_shouldHaveSameSize = 
    True ~=? length vec1 == length vec2 
        where 
            testPath1= packStartNode "Hello" : packEndNode "Leonhard" : []
            testPath2= packStartNode "Hello"  : []
            (vec1,vec2) = toVectors (testPath1,testPath2)

toVectors_onePathEmpty_shouldHaveSameSize = 
    True ~=? length vec1 == length vec2 
        where 
            testPath1=  []
            testPath2=  packStartNode "Hello" : packEndNode "Leonhard" : []
            (vec1,vec2) = toVectors (testPath1,testPath2)

toVectors_secondPathEmpty_shouldHaveSameSize = 
    True ~=? length vec1 == length vec2 
        where 
            testPath1= packStartNode "Hello" : packEndNode "Leonhard" : []
            testPath2= []
            (vec1,vec2) = toVectors (testPath1,testPath2)

toVectors_6distinctWords_shouldHaveLength6 = 
    6 ~=? length vec1 
        where 
            testPath1= packStartNode "Hello" : packNode "I" : packNode "am" : packEndNode "Leonhard" : []
            testPath2= packStartNode "Bye" : packEndNode "Visitor" : []
            (vec1,vec2) = toVectors (testPath1,testPath2)

toVectors_6distinctWords_otherVector_shouldHaveLength6Too = 
    6 ~=? length vec2 
        where 
            testPath1= packStartNode "Hello" : packNode "I" : packNode "am" : packEndNode "Leonhard" : []
            testPath2= packStartNode "Bye" : packEndNode "Visitor" : []
            (vec1,vec2) = toVectors (testPath1,testPath2)

toVectors_onePathEmpty_otherHas3Words_shouldHaveLength3 = 
    3 ~=? length vec1 
        where 
            testPath1= packStartNode "Hello" : packNode "my" : packEndNode "Opinosis" : []
            testPath2 = []
            (vec1,vec2) = toVectors (testPath1,testPath2)

toVectors_onePathEmpty_otherVector_otherHas3Words_shouldHaveLength3too = 
    3 ~=? length vec2
        where 
            testPath1= packStartNode "Hello" : packNode "my" : packEndNode "Opinosis" : []
            testPath2 = []
            (vec1,vec2) = toVectors (testPath1,testPath2)

cosineSim_samePaths_shouldBeOne = 
    True ~=? 1>= cosineSim testPath1 testPath2 && cosineSim testPath1 testPath2 >0.99999
        where 
            testPath1= packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPath2= packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []

cosineSim_CompleteDifferentPaths_shouldBeZero = 
    0 ~=? cosineSim testPath1 testPath2
        where 
            testPath1= packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPath2= packStartNode "Bye" : packNode "you" : packNode "fancy" :packEndNode "reader" : []

cosineSim_OneEmptyPath_shouldBeZero = 
    0 ~=? cosineSim testPath1 testPath2
        where 
            testPath1= packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPath2= []

cosineSim_TwoEmptyPath_shouldBeZero = 
    0 ~=? cosineSim testPath1 testPath2
        where 
            testPath1= []
            testPath2= []

cosineSim_FirstPathEmpty_shouldBeZero = 
    0 ~=? cosineSim testPath1 testPath2
        where 
            testPath1= []
            testPath2= packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []


jaccardSim_samePaths_shouldBeOne = 
    True ~=? 1>= jaccardSim testPath1 testPath2 && cosineSim testPath1 testPath2 >0.99999
        where 
            testPath1= packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPath2= packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []

jaccardSim_CompleteDifferentPaths_shouldBeZero = 
    0 ~=? jaccardSim testPath1 testPath2
        where 
            testPath1= packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPath2= packStartNode "Bye" : packNode "you" : packNode "fancy" :packEndNode "reader" : []

jaccardSim_secondPathEmpty_shouldBeZero = 
    0 ~=? jaccardSim testPath1 testPath2
        where 
            testPath1= packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []
            testPath2= []

jaccardSim_bothPathsEmpty_shouldBeZero = 
    0 ~=? jaccardSim testPath1 testPath2
        where 
            testPath1= []
            testPath2= []

jaccardSim_firstPathEmpty_shouldBeZero   = 
    0 ~=? jaccardSim testPath1 testPath2
        where 
            testPath1= []
            testPath2= packStartNode "Hello" : packNode "to" : packNode "my" :packEndNode "Test" : []

jaccardSim_oneOfTwoWordsOverlapping_shouldBePoint5 = 
    0.5 ~=? jaccardSim testPath1 testPath2 
        where 
            testPath1= packStartNode "Hello" : packEndNode "Leonhard" : []
            testPath2= packStartNode "Hello"  : []

-- same as above but with paths switched    
jaccardSim_oneOfTwoWordsOverlapping_shouldBeSymmetric = 
    0.5 ~=? jaccardSim testPath2 testPath1 
        where 
            testPath1= packStartNode "Hello" : packEndNode "Leonhard" : []
            testPath2= packStartNode "Hello"  : []