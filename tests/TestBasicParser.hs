module TestBasicParser where

import BasicParser
import Test.HUnit (Assertion, assertEqual, Testable (test), (@=?), (~=?))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Control.Applicative ( Alternative, (<|>), empty )
import Data.Char (isSpace)

testSuite :: Test
testSuite = testGroup "Basic Parser module" [
    do
        let actual = runParser (parseChar 'a') "abcd"
        let expected = Just ('a', "bcd")
        testCase "parseChar: First Char" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'z') "ybcd"
        let expected = Nothing
        testCase "parseChar: No Char in String" $ expected @=? actual,
    do
        let actual = runParser (parseChar 'z') ""
        let expected = Nothing
        testCase "parseChar: empty string" $ expected @=? actual,
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
        let actual = runParser (parseAnyChar "abc") ""
        let expected = Nothing
        testCase "parseAnyChar: empty string" $ expected @=? actual,
    do
        let actual = runParser (parseAnyChar "") "abc"
        let expected = Nothing
        testCase "parseAnyChar: empty needles" $ expected @=? actual,
    do
        let actual = runParser (parseString "") "abc"
        let expected = Just ("", "abc")
        testCase "parseString: empty" $ expected @=? actual,
    do
        let actual = runParser (parseString "true") "trueabc"
        let expected = Just ("true", "abc")
        testCase "parseString: filled string" $ expected @=? actual,
    do
        let actual = runParser (parseString "true") ""
        let expected = Nothing
        testCase "parseString: empty haystack" $ expected @=? actual,
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
        let actual = runParser parseInt "-123, 456" 
        let expected = Just (-123, ", 456")
        testCase "parseInt: parse negative number" $ expected @=? actual,
    do
        let actual = runParser parseFloat "42"
        let expected = Nothing
        testCase "parseFloat: parse positive integer (returns nothing)" $ expected @=? actual,
    do
        let actual = runParser parseFloat "-123Hello"
        let expected = Nothing
        testCase "parseFloat: parse negative integer (returns nothing)" $ expected @=? actual,
    do
        let actual = runParser parseFloat "1.234aHello"
        let expected = Just (1.234, "aHello")
        testCase "parseFloat: parse positive float" $ expected @=? actual,
    do
        let actual = runParser parseFloat "-42.10"
        let expected = Just (-42.1, "")
        testCase "parseFloat: parse negative float" $ expected @=? actual,
    do
        let actual = runParser parseFloat "-42.a10"
        let expected = Nothing
        testCase "parseFloat: Error after point" $ expected @=? actual,
    do
        let actual = runParser parseFloat "Hello World"
        let expected = Nothing
        testCase "parseFloat: not a number" $ expected @=? actual,
    do
        let actual = runParser parseWord "1 2"
        let expected = Just ("1", " 2")
        testCase "parseWord: parse first word" $ expected @=? actual,
    do
        let actual = runParser parseWord "  Hello  "
        let expected = Nothing
        testCase "parseWord: no word in first place" $ expected @=? actual,
    do
        let actual = runParser (parseN 0 $ parseIf isSpace) "   hello"
        let expected = Just ([], "   hello")
        testCase "parseN, zero time" $ expected @=? actual,
    do
        let actual = runParser (parseN 1 $ parseIf isSpace) "   hello"
        let expected = Just ([' '], "  hello")
        testCase "parseN, once" $ expected @=? actual,
    do
        let actual = runParser (parseN 3 $ parseIf isSpace) "   hello"
        let expected = Just ("   ", "hello")
        testCase "parseN, three times" $ expected @=? actual,
    do
        let actual = runParser (parseN 5 $ parseIf isSpace) "   "
        let expected = Nothing
        testCase "parseN, more than available" $ expected @=? actual,
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
        testCase "monadic parser >>= second fail" $ expected @=? actual,
    do
        let actual = runParser (parseMany (parseAnyChar "1234") <%> parseChar '1') "1234lol"
        let expected = Just ('1', "234lol")
        testCase "<%> succeed" $ expected @=? actual,
    do
        let actual = runParser (parseSome (parseAnyChar "234") <%> parseChar '1') "1234lol"
        let expected = Nothing
        testCase "<%> first fail" $ expected @=? actual,
    do
        let actual = runParser (parseSome (parseAnyChar "1234") <%> parseChar '2') "1234lol"
        let expected = Nothing
        testCase "<%> second fail" $ expected @=? actual,

    do
        let actual = runParser (parseAnyChar "234" <^> parseChar '1') "1234lol"
        let expected = Just ('1', "234lol")
        testCase "<^> firstFail" $ expected @=? actual,

    do
        let actual = runParser (parseAnyChar "1234" <^> parseChar 'a') "1234lol"
        let expected = Just ('1', "234lol")
        testCase "<^> second Fail" $ expected @=? actual,

    do
        let actual = runParser (parseAnyChar "1234" <^> parseChar '1') "1234lol"
        let expected = Nothing
        testCase "<^> both succeed" $ expected @=? actual,

    do
        let actual = runParser (parseAnyChar "a234" <^> parseChar 'a') "1234lol"
        let expected = Nothing
        testCase "<^> both fail" $ expected @=? actual,


    do
        let actual = runParser parseWord "1234 lol"
        let expected = Just ("1234", " lol")
        testCase "parseWord" $ expected @=? actual,
    do
        let actual = runParser parseWord "   1234 lol"
        let expected = Nothing
        testCase "parseWord, start with empty spaces" $ expected @=? actual,
    do
        let actual = runParser parseWord ""
        let expected = Nothing
        testCase "parseWord, empty string" $ expected @=? actual,
    do
        let actual = runParser parseWhitespaces "  \n\t  hello"
        let expected = Just("  \n\t  ", "hello")
        testCase "parseWhiteSpaces" $ expected @=? actual,
    do
        let actual = runParser parseWhitespaces ""
        let expected = Just("", "")
        testCase "parseWhiteSpaces, empty string" $ expected @=? actual,
    do
        let actual = runParser (parseScope ('(', ')')) "()hello"
        let expected = Just("", "hello")
        testCase "parseScope, empty content" $ expected @=? actual,
    do
        let actual = runParser (parseScope ('{', '}')) "{hello1}hello2"
        let expected = Just("hello1", "hello2")
        testCase "parseScope, string content" $ expected @=? actual,
    do
        let actual = runParser (parseScope ('[', ']')) "[hello1[1]]hello2"
        let expected = Just("hello1[1]", "hello2")
        testCase "parseScope, nested content" $ expected @=? actual,
    do
        let actual = runParser (parseScope ('[', ']')) "hello1[1]]hello2"
        let expected = Nothing
        testCase "parseScope, impaired content" $ expected @=? actual,
    do
        let actual = runParser (parseScope ('(', ')')) "hello2(hello1(1))"
        let expected = Nothing
        testCase "parseScope, not parenthesis" $ expected @=? actual
    -- do
    --     let actual = runParser empty "Hello"
    --     let expected = Nothing
    --     testCase "empty" $ expected @=? actual
    ]
