module TestParser where

import Parser
import Test.HUnit (Assertion, assertEqual, Testable (test))
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.HUnit (testCase)

testSuite :: Test
testSuite = testGroup "Parser module" [
    do
        let actual = runParser (parseChar 'a') "abcd"
        let expected = Just ('a', "bcd")
        testCase "parseChar: First Char" $ assertEqual "Parse First Char of string" expected actual,
    do
        let actual = runParser (parseChar 'z') "ybcd"
        let expected = Nothing
        testCase "parseChar: No Char in String" $ assertEqual "Parse Char not in string" expected actual,
    do
        let actual = runParser (parseChar 'b') "ybcd"
        let expected = Nothing
        testCase "parseChar: Second Char" $ assertEqual "Parse Second Char in string" expected actual,
    do
        let actual = runParser (parseChar 'a') "aaaa"
        let expected = Just ('a', "aaa")
        testCase "parseChar: Only First Char" $ assertEqual "Parse only first Char in string" expected actual,
    do
        let actual = runParser (parseAnyChar "bca") "abcd"
        let expected = Just ('a', "bcd")
        testCase "parseAnyChar: First Char" $ assertEqual "Parse first Char in string" expected actual,
    do
        let actual = runParser (parseAnyChar "xyz") "abcd"
        let expected = Nothing
        testCase "parseAnyChar: No Char in string" $ assertEqual "Parse Char when not in string" expected actual,
    do
        let actual = runParser (parseAnyChar "bca") "cdef"
        let expected = Just ('c', "def")
        testCase "parseAnyChar: Second Char in string" $ assertEqual "Parse Char fron second in string" expected actual,
    do
        let actual = runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd"
        let expected = Just ('a', "bcd")
        testCase "parseOr: First is Ok" $ assertEqual "ParseOr with First Valid" expected actual,
    do
        let actual = runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcda"
        let expected = Just ('b', "cda")
        testCase "parseOr: Second is Ok" $ assertEqual "ParseOr with Second Valid" expected actual,
    do
        let actual = runParser (parseOr (parseChar 'a') (parseChar 'b')) "xyz"
        let expected = Nothing
        testCase "parseOr: none is Ok" $ assertEqual "ParseOr with no Valid" expected actual,
    do
        let actual = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd"
        let expected = Just (('a','b'), "cd")
        testCase "parseAnd: all is Ok" $ assertEqual "ParseAnd with all Valid" expected actual,
    do
        let actual = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcda"
        let expected = Nothing
        testCase "parseAnd: first is invalid" $ assertEqual "ParseAnd with first invalid" expected actual,
    do
        let actual = runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd"
        let expected = Nothing
        testCase "parseAnd: second is invalid" $ assertEqual "ParseAnd with second invalid" expected actual
    ]


-- case_parseAndWith_example_1 :: Assertion
-- case_parseAndWith_example_1 = assertEqual "Example 1" expected actual
--     where
--         actual = runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "abcd"
--         expected = Just ("ab", "cd")

-- case_parseMany_example_1 :: Assertion
-- case_parseMany_example_1 = assertEqual "Example 1" expected actual
--     where
--         actual = runParser (parseMany (parseChar ' ')) "  foobar"
--         expected = Just ("  ", "foobar")

-- case_parseMany_example_2 :: Assertion
-- case_parseMany_example_2 = assertEqual "Example 2" expected actual
--     where
--         actual = runParser (parseMany (parseChar ' ')) "foobar  "
--         expected = Just ("", "foobar  ")

-- case_parseSome_example_1 :: Assertion
-- case_parseSome_example_1 = assertEqual "Example 1" expected actual
--     where
--         actual = runParser (parseSome (parseAnyChar ['0'..'9'])) "42foobar"
--         expected = Just ("42", "foobar")

-- case_parseSome_example_2 :: Assertion
-- case_parseSome_example_2 = assertEqual "Example 2" expected actual
--     where
--         actual = runParser (parseSome (parseAnyChar ['0'..'9'])) "foobar42"
--         expected = Nothing

-- case_parseInt_example :: Assertion
-- case_parseInt_example = assertEqual "Example 1" expected actual
--     where
--         actual  = runParser parseInt "123 ,456" 
--         expected = Just (123, " ,456")

-- case_parseWord :: Assertion
-- case_parseWord = assertEqual "Example 1" expected actual
--     where 
--       expected = Just ("1", " 2")
--       actual = runParser parseWord "1 2"

-- case_parseWord_frontSpace :: Assertion
-- case_parseWord_frontSpace = assertEqual "Example 1" expected actual
--     where 
--       expected = Nothing
--       actual = runParser parseWord "  Hello  "