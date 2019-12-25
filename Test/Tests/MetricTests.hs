{-# LANGUAGE OverloadedStrings #-}

module Tests.MetricTests(allMetricTests,allMetricProperties) where 

import Tests.TestSuite
import Test.HUnit hiding (Node,Path)

import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck(forAll, listOf, listOf1,Property)

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
    ]

allMetricProperties = [
    testProperty "cosineSim of same element is 1" prop_cosineSimReflexivity,
    testProperty "cosineSim of empty list is 0" prop_cosineSimEmptyElem,
    testProperty "cosineSim is symmetric" prop_cosineSimSymmetry,
    testProperty "jaccardSim to itself is 1" prop_jaccardSimReflexivity,
    testProperty "jaccardSim of empty list is 0" prop_jaccardSimEmptyElem,
    testProperty "jaccardSim is symmetric" prop_jaccardSimSymmetry
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

prop_cosineSimReflexivity :: Property
prop_cosineSimReflexivity = 
    forAll (listOf1 arbitrary) cosineSimReflexivity
    where 
        cosineSimReflexivity :: Path -> Bool
        cosineSimReflexivity p = 
            cosineSim p p == 1.0 

prop_cosineSimEmptyElem p = 
    cosineSim p [] == 0

prop_cosineSimSymmetry p1 p2 = 
    cosineSim p1 p2 == cosineSim p2 p1

prop_jaccardSimReflexivity :: Property
prop_jaccardSimReflexivity = 
    forAll (listOf1 arbitrary) jaccardSimReflexivity
    where 
        jaccardSimReflexivity :: Path -> Bool
        jaccardSimReflexivity p = 
            jaccardSim p p == 1.0 

prop_jaccardSimEmptyElem p = 
    jaccardSim p [] == 0

prop_jaccardSimSymmetry p1 p2 = 
    jaccardSim p1 p2 == jaccardSim p2 p1