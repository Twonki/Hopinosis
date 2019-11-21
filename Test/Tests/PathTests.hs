{-# LANGUAGE OverloadedStrings #-}


module Tests.PathTests where 

import Tests.TestSuite
import Test.HUnit hiding (Node)

import qualified Data.Map.Monoidal.Strict as Map
import Data.Text hiding(map,singleton,foldr,words,null,length,head)
    
import Tests.TestSuite

allPathTests = TestList [
    TestLabel "exposed PathFunctions" pathTests
    , TestLabel "Internal PathFunctions" pathInternalTests
    ]


pathTests = TestList [
    TestLabel "getStarts_ofOneDocument_shouldBeOne" getStarts_ofOneDocument_shouldBeOne
    ,TestLabel "getStarts_ofOneWord_shouldBeWord" getStarts_ofOneWord_shouldBeWord
    ,TestLabel "getStarts_ofEmpty_shouldBeEmpty" getStarts_ofEmpty_shouldBeEmpty
    ,TestLabel "getStarts_ofTwoDocuments_shouldBeTwo" getStarts_ofTwoDocuments_shouldBeTwo
    ,TestLabel "getStarts_ofTwoDocumentsWithSameStart_shouldBeOne" getStarts_ofTwoDocumentsWithSameStart_shouldBeOne
    ]

getStarts_ofOneDocument_shouldBeOne = 
    1 ~=? length (starts (toGraphOne "Hello"))

getStarts_ofOneWord_shouldBeWord = 
    "Hello" ~=? (fst . head) (starts (toGraphOne "Hello"))

getStarts_ofEmpty_shouldBeEmpty = 
    True ~=? null ( starts (toGraphOne []))

getStarts_ofTwoDocuments_shouldBeTwo = 
    2 ~=? length (starts (toGraphMany ["Hello my darling test", "Goodbye my darling test"]))

getStarts_ofTwoDocumentsWithSameStart_shouldBeOne =
    1 ~=? length (starts (toGraphMany ["Hello my darling test", "Hello my other darling test"]))


pathInternalTests = TestList [

    ]
