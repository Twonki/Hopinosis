{-# LANGUAGE OverloadedStrings #-}

module Tests.NodeTests(allNodeTests,allNodeProperties) where 

import Tests.TestSuite
import Test.HUnit hiding (Node)

import Test.Framework.Providers.QuickCheck2

import qualified Data.Map.Monoidal.Strict as Map
import Data.Text hiding(map,singleton,foldr)

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

allNodeProperties = [
     testProperty "v+mempty=v" prop_memptyAddition
    , testProperty "value addition" prop_valueAddition
    ]

--prop = uncurry testProperty

--nodeQuickCheckProps = [named_prop_memptyAddition,named_prop_valueAddition]

testValueMerge_ShouldBeAdded = (Values 2 Map.empty 0 False) ~=? mappend uniValue uniValue
testValueMerge_EmptyValueShould_BeNeutral = uniValue ~=? mappend uniValue emptyValues
testValueMerge_TwoEmptyValues_ShouldBeEmpty = emptyValues ~=? mappend emptyValues emptyValues

testValueMerge_oneStartValue_shouldBeStartValue = 
    1 ~=? starts (mappend startValue uniValue)
testValueMerge_oneEndValue_shouldBeEndValue = 
    True ~=? validEnd (mappend endValue uniValue)
testValueMerge_oneStartValue_onEmptyValue_shouldBeStartValue = 
    1 ~=? starts (mappend startValue emptyValues)

testValueMerge_oneOut_mergedShouldBeTheOutOfFirst =
    mFromList [("Sample",1)] ~=? outs (endValue <> (oneOut "Sample"))
testValueMerge_oneOut_oneEmpty_mergedShouldBeTheOutOfFirst =
    mFromList [("Sample",1)] ~=? outs (emptyValues <> (oneOut "Sample"))
testValueMerge_twoOut_oneEmpty_shouldBeTwoOutOfFirst=
    mFromList [("Apple",1),("Banana",1)] ~=? outs (mappend endValue (Values 1 (mFromList [("Apple",1),("Banana",1)]) 0 False))
testValueMerge_oneOut_oneOut_shouldBeTwoOutOfBoth=
    mFromList [("Apple",1),("Banana",1)] ~=? outs (mappend (oneOut "Apple") (oneOut "Banana"))
testValueMerge_oneOut_oneOut_sameValue_shouldBeMergedAndIncreased = 
    mFromList [("Sample",2)] ~=? outs (mappend (oneOut "Sample") (oneOut "Sample"))
testValueMerge_oneOut_oneOutWithMag2_sameValue_shouldBeMergedAndIncreased = 
    mFromList [("Sample",3)] ~=? outs (mappend (oneOut "Sample") (Values 1 (Map.singleton "Sample" 2 ) 0 False))
    

testFoldValues_threeUnivalues_shouldHaveMag3 = 
    (Values 3 Map.empty 0 False) ~=? mconcat [uniValue,uniValue,uniValue]
testFoldValues_EmptyList_shouldBeEmptyValues = 
    emptyValues ~=? mconcat []
testFoldValues_WithStart_shouldBeStart = 
    1 ~=? starts (mconcat [startValue])
testFoldValues_WithEnd_shouldBeEnd =
    True ~=? validEnd (mconcat [endValue])
testFoldValues_withEmptyValues_shouldBeEmptyValues =
    emptyValues ~=? mconcat [emptyValues,emptyValues]

testFoldValues_withOuts_shouldHaveOuts = 
    mFromList [("Sample",1)] ~=? outs (mconcat [oneOut "Sample"])
testFoldValues_withOuts_twiceSame_shouldHaveIncreasedOuts =
    mFromList [("Sample",2)] ~=? outs (mconcat [oneOut "Sample",oneOut "Sample"])
testFoldValues_withOuts_threeTimesSame_shouldHaveIncreasedOuts =
    mFromList [("Sample",3)] ~=? outs (mconcat [oneOut "Sample",oneOut "Sample",oneOut "Sample"])
testFoldValues_withTwoDifferentOuts_shouldBeBoth = 
    mFromList [("Apple",1),("Banana",1)] ~=? outs (mconcat [oneOut "Apple",oneOut "Banana"])

prop_memptyAddition :: Values  -> Bool
prop_memptyAddition v = 
    v <> mempty == v

prop_valueAddition :: Values -> Values -> Bool    
prop_valueAddition v1@(Values a1 b1 c1 d1) v2@(Values a2 b2 c2 d2) =
    v1 <> v2 == Values (a1 + a2) (Map.unionWith (+) b1 b2) (c1 + c2) (d1||d2)

oneOut :: Text -> Values
oneOut s = Values 1 (Map.singleton s 1) 0 False

emptyValues = mempty

mFromList :: [(Text,Int)] -> Map.MonoidalMap Text Int
mFromList = foldr (Map.unionWith (+) . uncurry Map.singleton) Map.empty