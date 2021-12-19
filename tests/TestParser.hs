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
        testCase "parseAnd: second is invalid" $ assertEqual "ParseAnd with second invalid" expected actual,
    do
        let actual = runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "abcd"
        let expected = Just ("ab", "cd")
        testCase "parseAndWith: merge results" $ assertEqual "ParseAndwith with valid returns" expected actual,
    do
        let actual = runParser (parseMany (parseChar ' ')) "  foobar"
        let expected = Just ("  ", "foobar")
        testCase "parseMany: remove leading spaces" $ assertEqual "ParseMany remove leading spaces" expected actual,
    do
        let actual = runParser (parseMany (parseChar ' ')) "foobar  "
        let expected = Just ("", "foobar  ")
        testCase "parseMany: remove leading spaces when none" $ assertEqual "ParseMany remove no spaces" expected actual,
    do
        let actual = runParser (parseSome (parseAnyChar ['0'..'9'])) "42foobar"
        let expected = Just ("42", "foobar")
        testCase "parseSome: parseNumber" $ assertEqual "ParseMany parse first digits" expected actual,
    do
        let actual = runParser (parseSome (parseAnyChar ['0'..'9'])) "foobar42"
        let expected = Nothing
        testCase "parseSome: parse number when none" $ assertEqual "parseSome parse first digits" expected actual,
    do
        let actual = runParser parseInt "123 ,456" 
        let expected = Just (123, " ,456")
        testCase "parseInt: parse first number" $ assertEqual "parseInt parse first digits" expected actual,
    do
        let actual = runParser parseWord "1 2"
        let expected = Just ("1", " 2")
        testCase "parseWord: parse first word" $ assertEqual "parse first word" expected actual,
    do
        let actual = runParser parseWord "  Hello  "
        let expected = Nothing
        testCase "parseWord: no word in first place" $ assertEqual "returns nothing" expected actual
    ]
