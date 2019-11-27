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