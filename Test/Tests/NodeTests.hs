module Tests.NodeTests where 

import Tests.TestSuite
import Test.HUnit hiding (Node)

allNodeTests = TestList [
    TestLabel "valueMerge_UniValues_ShouldHaveMag2" testValueMergeShouldBeAdded
    , TestLabel "valueMerge_EmptyValue_ShouldBeNeutralElement" testValueMergeEmptyValueShouldBeNeutral
    , TestLabel "valueMerge_TwoEmptyValues_ShouldBeEmptyValues" testValueMergeTwoEmptyValuesShouldBeEmpty
    ]

testValueMergeShouldBeAdded = (Values 2 [] False False) ~=? mergeValues uniValue uniValue
testValueMergeEmptyValueShouldBeNeutral = uniValue ~=? mergeValues uniValue emptyValues
testValueMergeTwoEmptyValuesShouldBeEmpty = emptyValues ~=? mergeValues emptyValues emptyValues