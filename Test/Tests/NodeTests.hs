{-# LANGUAGE OverloadedStrings #-}

module Tests.NodeTests where 

import Tests.TestSuite
import Test.HUnit hiding (Node)

import qualified Data.Map as Map
import Data.Text hiding(map,singleton)

allNodeTests = TestList [
    -- Magnitude and Neutrality
    TestLabel "valueMerge_UniValues_ShouldHaveMag2" testValueMerge_ShouldBeAdded
    , TestLabel "valueMerge_EmptyValue_ShouldBeNeutralElement" testValueMerge_EmptyValueShould_BeNeutral
    , TestLabel "valueMerge_TwoEmptyValues_ShouldBeEmptyValues" testValueMerge_TwoEmptyValues_ShouldBeEmpty
    -- Start End Concatenation
    , TestLabel "testValueMerge_oneStartValue_shouldBeStartValue" testValueMerge_oneStartValue_shouldBeStartValue
    , TestLabel "testValueMerge_oneEndValue_shouldBeEndValue" testValueMerge_oneEndValue_shouldBeEndValue
    , TestLabel "testValueMerge_oneStartValue_onEmptyValue_shouldBeStartValue" testValueMerge_oneStartValue_onEmptyValue_shouldBeStartValue
    -- Out Concatenation
    , TestLabel "testValueMerge_oneOut_mergedShouldBeTheOutOfFirst" testValueMerge_oneOut_mergedShouldBeTheOutOfFirst
    , TestLabel "testValueMerge_oneOut_oneEmpty_mergedShouldBeTheOutOfFirst" testValueMerge_oneOut_oneEmpty_mergedShouldBeTheOutOfFirst
    , TestLabel "testValueMerge_twoOut_oneEmpty_shouldBeTwoOutOfFirst" testValueMerge_twoOut_oneEmpty_shouldBeTwoOutOfFirst
    , TestLabel "testValueMerge_oneOut_oneOut_shouldBeTwoOutOfBoth" testValueMerge_oneOut_oneOut_shouldBeTwoOutOfBoth
    , TestLabel "testValueMerge_oneOut_oneOut_sameValue_shouldBeMergedAndIncreased" testValueMerge_oneOut_oneOut_sameValue_shouldBeMergedAndIncreased
    , TestLabel "testValueMerge_oneOut_oneOutWithMag2_sameValue_shouldBeMergedAndIncreased" testValueMerge_oneOut_oneOutWithMag2_sameValue_shouldBeMergedAndIncreased
    -- Fold Attributes
    , TestLabel "testFoldValues_threeUnivalues_shouldHaveMag3" testFoldValues_threeUnivalues_shouldHaveMag3
    , TestLabel "testFoldValues_EmptyList_shouldBeEmptyValues" testFoldValues_EmptyList_shouldBeEmptyValues
    , TestLabel "testFoldValues_WithStart_shouldBeStart" testFoldValues_WithStart_shouldBeStart
    , TestLabel "testFoldValues_WithEnd_shouldBeEnd" testFoldValues_WithEnd_shouldBeEnd
    , TestLabel "testFoldValues_withEmptyValues_shouldBeEmptyValues" testFoldValues_withEmptyValues_shouldBeEmptyValues
    -- Fold Out Concatenation
    , TestLabel "testFoldValues_withOuts_shouldHaveOuts" testFoldValues_withOuts_shouldHaveOuts
    , TestLabel "testFoldValues_withOuts_twiceSame_shouldHaveIncreasedOuts" testFoldValues_withOuts_twiceSame_shouldHaveIncreasedOuts
    , TestLabel "testFoldValues_withOuts_threeTimesSame_shouldHaveIncreasedOuts" testFoldValues_withOuts_threeTimesSame_shouldHaveIncreasedOuts
    , TestLabel "testFoldValues_withTwoDifferentOuts_shouldBeBoth" testFoldValues_withTwoDifferentOuts_shouldBeBoth
    ]

testValueMerge_ShouldBeAdded = (Values 2 Map.empty False False) ~=? mappend uniValue uniValue
testValueMerge_EmptyValueShould_BeNeutral = uniValue ~=? mappend uniValue emptyValues
testValueMerge_TwoEmptyValues_ShouldBeEmpty = emptyValues ~=? mappend emptyValues emptyValues

testValueMerge_oneStartValue_shouldBeStartValue = 
    True ~=? validStart (mappend startValue uniValue)
testValueMerge_oneEndValue_shouldBeEndValue = 
    True ~=? validEnd (mappend endValue uniValue)
testValueMerge_oneStartValue_onEmptyValue_shouldBeStartValue = 
    True ~=? validStart (mappend startValue emptyValues)

testValueMerge_oneOut_mergedShouldBeTheOutOfFirst =
    Map.fromList [("Sample",1)] ~=? outs (endValue <> (oneOut "Sample"))
testValueMerge_oneOut_oneEmpty_mergedShouldBeTheOutOfFirst =
    Map.fromList [("Sample",1)] ~=? outs (emptyValues <> (oneOut "Sample"))
testValueMerge_twoOut_oneEmpty_shouldBeTwoOutOfFirst=
    Map.fromList [("Apple",1),("Banana",1)] ~=? outs (mappend endValue (Values 1 (Map.fromList [("Apple",1),("Banana",1)]) False False))
testValueMerge_oneOut_oneOut_shouldBeTwoOutOfBoth=
    Map.fromList [("Apple",1),("Banana",1)] ~=? outs (mappend (oneOut "Apple") (oneOut "Banana"))
testValueMerge_oneOut_oneOut_sameValue_shouldBeMergedAndIncreased = 
    Map.fromList [("Sample",2)] ~=? outs (mappend (oneOut "Sample") (oneOut "Sample"))
testValueMerge_oneOut_oneOutWithMag2_sameValue_shouldBeMergedAndIncreased = 
    Map.fromList [("Sample",3)] ~=? outs (mappend (oneOut "Sample") (Values 1 (Map.singleton "Sample" 2 ) False False))
    

testFoldValues_threeUnivalues_shouldHaveMag3 = 
    (Values 3 Map.empty False False) ~=? mconcat [uniValue,uniValue,uniValue]
testFoldValues_EmptyList_shouldBeEmptyValues = 
    emptyValues ~=? mconcat []
testFoldValues_WithStart_shouldBeStart = 
    True ~=? validStart (mconcat [startValue])
testFoldValues_WithEnd_shouldBeEnd =
    True ~=? validEnd (mconcat [endValue])
testFoldValues_withEmptyValues_shouldBeEmptyValues =
    emptyValues ~=? mconcat [emptyValues,emptyValues]

testFoldValues_withOuts_shouldHaveOuts = 
    Map.fromList [("Sample",1)] ~=? outs (mconcat [oneOut "Sample"])
testFoldValues_withOuts_twiceSame_shouldHaveIncreasedOuts =
    Map.fromList [("Sample",2)] ~=? outs (mconcat [oneOut "Sample",oneOut "Sample"])
testFoldValues_withOuts_threeTimesSame_shouldHaveIncreasedOuts =
    Map.fromList [("Sample",3)] ~=? outs (mconcat [oneOut "Sample",oneOut "Sample",oneOut "Sample"])
testFoldValues_withTwoDifferentOuts_shouldBeBoth = 
    Map.fromList [("Apple",1),("Banana",1)] ~=? outs (mconcat [oneOut "Apple",oneOut "Banana"])

oneOut :: Text -> Values
oneOut s = Values 1 (Map.singleton s 1) False False