{-# LANGUAGE OverloadedStrings #-}

module Tests.GraphTests where 

import Tests.TestSuite

import Tests.TestSuite
import Test.HUnit hiding (Node)

import qualified Data.Map.Monoidal.Strict as Map
import Data.Text hiding(map,singleton,filter,length)

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
    
    ,TestLabel "testBugSentence_shouldBeParsed_shouldTerminate" testBugSentence_HasenDieBlasen
    ,TestLabel "testBugDocument_shouldBeParsed_shouldTerminate" testBugDocument_HasenDieBlasen
    ,TestLabel "testBugDocument_HasenDieBlasen2" testBugDocument_HasenDieBlasen2

    ,TestLabel "testBugDocument_4longSentences_shouldHave10Keys" testBugDocument_4longSentences_shouldHave10Keys
    ,TestLabel "testBugDocument_4longSentences_shouldHave4Ends" testBugDocument_4longSentences_shouldHave4Ends
    ,TestLabel "testBugDocument_4longSentences_shouldHave3Starts" testBugDocument_4longSentences_shouldHave3Starts
    ]


testParseEmpty_shouldBeEmpty = 
    True ~=? Map.null (parseSentence [])
testParseOneWord_shouldBeThere = 
    Just (Values 1 Map.empty 1 True) ~=? Map.lookup "Hello" (parseSentence ["Hello"] )

testParseTwoWords_checkLastOne =
    Just (Values 1 Map.empty 0 True) ~=? Map.lookup "Bye" (parseSentence ["Hello","Bye"])
testParseTwoWords_checkFirstOne =
    Just (Values 1 (Map.singleton "Bye" 1) 1 False) ~=? Map.lookup "Hello" (parseSentence ["Hello","Bye"])

testParseThreeWords_checkFirstOne =
    Just (Values 1 (Map.singleton "Middle" 1) 1 False) ~=? Map.lookup "Hello" (parseSentence ["Hello","Middle","Bye"])
testParseThreeWords_checkLastOne =
    Just (Values 1 Map.empty 0 True) ~=? Map.lookup "Bye" (parseSentence ["Hello","Middle","Bye"])
testParseThreeWords_checkMiddleOne = 
    Just (Values 1 (Map.singleton "Bye" 1) 0 False) ~=? Map.lookup "Middle" (parseSentence ["Hello","Middle","Bye"])

-- Word-DUplication-Tests
testParseWordTwice_check =
    Just (Values 2 (Map.singleton "Hey" 1) 1 True) ~=? Map.lookup "Hey" (parseSentence ["Hey","Hey"])
testParseWordTwice_withOtherWordInBetween_check =
    Just (Values 2 (Map.singleton "Other" 1) 1 True) ~=? Map.lookup "Hey" (parseSentence ["Hey","Other","Hey"])
testParseWordThrice_check =
    Just (Values 3 (Map.singleton "Hey" 2) 1 True) ~=? Map.lookup "Hey" (parseSentence ["Hey","Hey","Hey"])

-- Document-Duplication-Tests
testParseDocumentTwice_check =
    Just (Values 2 Map.empty 2 True) ~=? Map.lookup "Hey" (parseDocument [["Hey"],["Hey"]])
testParseDocumentThrice_check =
    Just (Values 3 Map.empty 3 True) ~=? Map.lookup "Hey" (parseDocument [["Hey"],["Hey"],["Hey"]])

testParseDocuments_WordsAreCombinedOverDocuments = 
    Just (Values 2 (Map.singleton "Hey" 1) 1 True) ~=? Map.lookup "Baby" (parseDocument [["Hey","Baby"],["Baby","Hey"]])

    
testParseEmptyDocument_shouldBeEmpty = 
    True ~=? Map.null (parseDocument [])
testParseDocument_singleWord_shouldBeThere = 
    Just (Values 1 Map.empty 1 True) ~=? Map.lookup "Hello" (parseDocument [["Hello"]] )

{-
    Defensive Tests - Created to avoid Regression Bugs
-}
testBugSentence_HasenDieBlasen = 
    5 ~=? Prelude.length ( Map.keys (parseSentence (map pack $ Prelude.words "Ich mag Hasen die blasen")))
testBugDocument_HasenDieBlasen = 
    5 ~=? Prelude.length ( Map.keys (parseDocument [(map pack $ Prelude.words "Ich mag Hasen die blasen")]))
testBugDocument_HasenDieBlasen2 = 
    5 ~=? Prelude.length ( Map.keys (parseDocument [(map pack $ Prelude.words "Ich mag Hasen die blasen"),(map pack $ Prelude.words "Ich mag Hasen die blasen")]))
    

{-
    These Values have been collected using Pen and Paper
-}
testBugDocument_4longSentences_shouldHave10Keys = 
    10 ~=? length (Map.keys testGraph)
        where testGraph = toGraphMany ["Hello I like dogs","I like rabbits","You are different","You hate rabbits","You like me"]

testBugDocument_4longSentences_shouldHave4Ends=
    4 ~=? length (( filter (\(k,v) -> validEnd v) . Map.assocs) testGraph)
        where testGraph = toGraphMany ["Hello I like dogs","I like rabbits","You are different","You hate rabbits","You like me"]

testBugDocument_4longSentences_shouldHave3Starts=
    3 ~=? length ((filter (\(k,v) -> starts v > 0) . Map.assocs) testGraph)
        where testGraph = toGraphMany ["Hello I like dogs","I like rabbits","You are different","You hate rabbits","You like me"]
