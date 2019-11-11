module Tests.NodeTests where 

import Tests.TestSuite
import Test.HUnit hiding (Node)

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

testValueMerge_ShouldBeAdded = (Values 2 [] False False) ~=? mergeValues uniValue uniValue
testValueMerge_EmptyValueShould_BeNeutral = uniValue ~=? mergeValues uniValue emptyValues
testValueMerge_TwoEmptyValues_ShouldBeEmpty = emptyValues ~=? mergeValues emptyValues emptyValues

testValueMerge_oneStartValue_shouldBeStartValue = 
    True ~=? validStart (mergeValues startValue uniValue)
testValueMerge_oneEndValue_shouldBeEndValue = 
    True ~=? validEnd (mergeValues endValue uniValue)
testValueMerge_oneStartValue_onEmptyValue_shouldBeStartValue = 
    True ~=? validStart (mergeValues startValue emptyValues)

testValueMerge_oneOut_mergedShouldBeTheOutOfFirst =
    [("Sample",1)] ~=? outs (mergeValues endValue (Values 1 [("Sample",1)] False False))
testValueMerge_oneOut_oneEmpty_mergedShouldBeTheOutOfFirst =
    [("Sample",1)] ~=? outs (mergeValues emptyValues (Values 1 [("Sample",1)] False False))
testValueMerge_twoOut_oneEmpty_shouldBeTwoOutOfFirst=
    [("Apple",1),("Banana",1)] ~=? outs (mergeValues endValue (Values 1 [("Apple",1),("Banana",1)] False False))
testValueMerge_oneOut_oneOut_shouldBeTwoOutOfBoth=
    [("Apple",1),("Banana",1)] ~=? outs (mergeValues (Values 1 [("Apple",1)] False False) (Values 1 [("Banana",1)] False False))
testValueMerge_oneOut_oneOut_sameValue_shouldBeMergedAndIncreased = 
    [("Sample",2)] ~=? outs (mergeValues (Values 1 [("Sample",1)] False False) (Values 1 [("Sample",1)] False False))
testValueMerge_oneOut_oneOutWithMag2_sameValue_shouldBeMergedAndIncreased = 
    [("Sample",3)] ~=? outs (mergeValues (Values 1 [("Sample",1)] False False) (Values 1 [("Sample",2)] False False))
    

testFoldValues_threeUnivalues_shouldHaveMag3 = 
    (Values 3 [] False False) ~=? foldValues [uniValue,uniValue,uniValue]
testFoldValues_EmptyList_shouldBeEmptyValues = 
    emptyValues ~=? foldValues []
testFoldValues_WithStart_shouldBeStart = 
    True ~=? validStart (foldValues [startValue])
testFoldValues_WithEnd_shouldBeEnd =
    True ~=? validEnd (foldValues [endValue])
testFoldValues_withEmptyValues_shouldBeEmptyValues =
    emptyValues ~=? foldValues [emptyValues,emptyValues]

testFoldValues_withOuts_shouldHaveOuts = 
    [("Sample",1)] ~=? outs (foldValues [Values 1 [("Sample",1)] False False])
testFoldValues_withOuts_twiceSame_shouldHaveIncreasedOuts =
    [("Sample",2)] ~=? outs (foldValues [(Values 1 [("Sample",1)] False False),(Values 1 [("Sample",1)] False False)])
testFoldValues_withOuts_threeTimesSame_shouldHaveIncreasedOuts =
    [("Sample",3)] ~=? outs (foldValues [(Values 1 [("Sample",1)] False False),(Values 1 [("Sample",1)] False False),(Values 1 [("Sample",1)] False False)])
testFoldValues_withTwoDifferentOuts_shouldBeBoth = 
    [("Apple",1),("Banana",1)] ~=? outs (foldValues [(Values 1 [("Apple",1)] False False),(Values 1 [("Banana",1)] False False)])