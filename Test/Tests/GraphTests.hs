{-# LANGUAGE OverloadedStrings #-}

module Tests.GraphTests where 

import Tests.TestSuite

import Tests.TestSuite
import Test.HUnit hiding (Node)

import qualified Data.Map as Map
import Data.Text hiding(map,singleton)

allGraphTests = TestList [
    TestLabel "testParseEmpty_shouldBeEmpty" testParseEmpty_shouldBeEmpty
    ,TestLabel "testParseOneWord_shouldBeThere" testParseOneWord_shouldBeThere
    ,TestLabel "testParseTwoWords_checkLastOne" testParseTwoWords_checkLastOne
    ,TestLabel "testParseTwoWords_checkFirstOne" testParseTwoWords_checkFirstOne
    ,TestLabel "testParseThreeWords_checkFirstOne" testParseThreeWords_checkFirstOne
    ,TestLabel "testParseThreeWords_checkMiddleOne" testParseThreeWords_checkMiddleOne
    ,TestLabel "testParseThreeWords_checkLastOne" testParseThreeWords_checkLastOne
    ,TestLabel "testParseWordTwice_check" testParseWordTwice_check
    ,TestLabel "testParseWordThrice_check" testParseWordThrice_check
    ,TestLabel "testParseWordTwice_withOtherWordInBetween_check" testParseWordTwice_withOtherWordInBetween_check
    ,TestLabel "testParseDocumentTwice_check" testParseDocumentTwice_check
    ,TestLabel "testParseDocumentThrice_check" testParseDocumentThrice_check
    ,TestLabel "testParseEmptyDocument_shouldBeEmpty" testParseEmptyDocument_shouldBeEmpty
    ,TestLabel "testParseDocument_singleWord_shouldBeThere" testParseDocument_singleWord_shouldBeThere
    ,TestLabel "testParseDocuments_WordsAreCombinedOverDocuments" testParseDocuments_WordsAreCombinedOverDocuments
    ]


testParseEmpty_shouldBeEmpty = 
    True ~=? Map.null (parseSentence [])
testParseOneWord_shouldBeThere = 
    Just (Values 1 Map.empty True True) ~=? Map.lookup "Hello" (parseSentence ["Hello"] )

testParseTwoWords_checkLastOne =
    Just (Values 1 Map.empty False True) ~=? Map.lookup "Bye" (parseSentence ["Hello","Bye"])
testParseTwoWords_checkFirstOne =
    Just (Values 1 (Map.singleton "Bye" 1) True False) ~=? Map.lookup "Hello" (parseSentence ["Hello","Bye"])

testParseThreeWords_checkFirstOne =
    Just (Values 1 (Map.singleton "Middle" 1) True False) ~=? Map.lookup "Hello" (parseSentence ["Hello","Middle","Bye"])
testParseThreeWords_checkLastOne =
    Just (Values 1 Map.empty False True) ~=? Map.lookup "Bye" (parseSentence ["Hello","Middle","Bye"])
testParseThreeWords_checkMiddleOne = 
    Just (Values 1 (Map.singleton "Bye" 1) False False) ~=? Map.lookup "Middle" (parseSentence ["Hello","Middle","Bye"])

-- Word-DUplication-Tests
testParseWordTwice_check =
    Just (Values 2 (Map.singleton "Hey" 1) True True) ~=? Map.lookup "Hey" (parseSentence ["Hey","Hey"])
testParseWordTwice_withOtherWordInBetween_check =
    Just (Values 2 (Map.singleton "Other" 1) True True) ~=? Map.lookup "Hey" (parseSentence ["Hey","Other","Hey"])
testParseWordThrice_check =
    Just (Values 3 (Map.singleton "Hey" 2) True True) ~=? Map.lookup "Hey" (parseSentence ["Hey","Hey","Hey"])

-- Document-Duplication-Tests
testParseDocumentTwice_check =
    Just (Values 2 Map.empty True True) ~=? Map.lookup "Hey" (parseDocument [["Hey"],["Hey"]])
testParseDocumentThrice_check =
    Just (Values 3 Map.empty True True) ~=? Map.lookup "Hey" (parseDocument [["Hey"],["Hey"],["Hey"]])

testParseDocuments_WordsAreCombinedOverDocuments = 
    Just (Values 2 (Map.singleton "Hey" 1) True True) ~=? Map.lookup "Baby" (parseDocument [["Hey","Baby"],["Baby","Hey"]])

    
testParseEmptyDocument_shouldBeEmpty = 
    True ~=? Map.null (parseDocument [])
testParseDocument_singleWord_shouldBeThere = 
    Just (Values 1 Map.empty True True) ~=? Map.lookup "Hello" (parseDocument [["Hello"]] )
