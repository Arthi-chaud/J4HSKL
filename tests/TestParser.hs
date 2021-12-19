module TestParser where

import Parser
import Test.HUnit (Assertion, assertEqual, Testable (test), (@=?), (~=?))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Control.Applicative ( Alternative((<|>), empty) )

testSuite :: Test
testSuite = testGroup "Parser module" [
    do
        let actual = runParser (parseChar 'a') "abcd"
        let expected = Just ('a', "bcd")
        testCase "parseChar: First Char" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'z') "ybcd"
        let expected = Nothing
        testCase "parseChar: No Char in String" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'b') "ybcd"
        let expected = Nothing
        testCase "parseChar: Second Char" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'a') "aaaa"
        let expected = Just ('a', "aaa")
        testCase "parseChar: Only First Char" $ expected @=? actual,
    do
        let actual = runParser (parseAnyChar "bca") "abcd"
        let expected = Just ('a', "bcd")
        testCase "parseAnyChar: First Char" $ expected @=? actual,
    do
        let actual = runParser (parseAnyChar "xyz") "abcd"
        let expected = Nothing
        testCase "parseAnyChar: No Char in string" $ expected @=? actual,
    do
        let actual = runParser (parseAnyChar "bca") "cdef"
        let expected = Just ('c', "def")
        testCase "parseAnyChar: Second Char in string" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'a' <|> parseChar 'b') "abcd"
        let expected = Just ('a', "bcd")
        testCase "parseOr: First is Ok" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'a' <|> parseChar 'b') "bcda"
        let expected = Just ('b', "cda")
        testCase "parseOr: Second is Ok" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'a' <|> parseChar 'b') "xyz"
        let expected = Nothing
        testCase "parseOr: none is Ok" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'a' <&> parseChar 'b') "abcd"
        let expected = Just (('a','b'), "cd")
        testCase "parseAnd: all is Ok" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'a' <&> parseChar 'b') "bcda"
        let expected = Nothing
        testCase "parseAnd: first is invalid" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'a' <&> parseChar 'b') "acd"
        let expected = Nothing
        testCase "parseAnd: second is invalid" $ expected @=? actual,
    do
        let actual = runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "abcd"
        let expected = Just ("ab", "cd")
        testCase "parseAndWith: merge results" $ expected @=? actual,
    do
        let actual = runParser (parseMany (parseChar ' ')) "  foobar"
        let expected = Just ("  ", "foobar")
        testCase "parseMany: remove leading spaces" $ expected @=? actual,
    do
        let actual = runParser (parseMany (parseChar ' ')) "foobar  "
        let expected = Just ("", "foobar  ")
        testCase "parseMany: remove leading spaces when none" $ expected @=? actual,
    do
        let actual = runParser (parseSome (parseAnyChar ['0'..'9'])) "42foobar"
        let expected = Just ("42", "foobar")
        testCase "parseSome: parseNumber" $ expected @=? actual,
    do
        let actual = runParser (parseSome (parseAnyChar ['0'..'9'])) "foobar42"
        let expected = Nothing
        testCase "parseSome: parse number when none" $ expected @=? actual,
    do
        let actual = runParser parseInt "123 ,456" 
        let expected = Just (123, " ,456")
        testCase "parseInt: parse first number" $ expected @=? actual,
    do
        let actual = runParser parseWord "1 2"
        let expected = Just ("1", " 2")
        testCase "parseWord: parse first word" $ expected @=? actual,
    do
        let actual = runParser parseWord "  Hello  "
        let expected = Nothing
        testCase "parseWord: no word in first place" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'l' >>= parseChar) "llo"
        let expected = Just ('l', "o")
        testCase "monadic parser >>=" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'o' >>= parseChar) "llo"
        let expected = Nothing
        testCase "monadic parser >>= first fail" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'l' >>= parseChar) "lol"
        let expected = Nothing
        testCase "monadic parser >>= second fail" $ expected @=? actual
    -- do
    --     let actual = runParser empty "Hello"
    --     let expected = Nothing
    --     testCase "empty" $ expected @=? actual
    ]
