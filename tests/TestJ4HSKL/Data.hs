module TestJ4HSKL.Data where

import J4HSKL.Data
import Test.HUnit (Assertion, Testable (test), (@=?))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Text.Printf (printf)

testSuite :: Test
testSuite = testGroup "JSON Data module" [
    do
        let actual = show $ String "Hello World"
        let expected ="\"Hello World\""
        testCase "Show JSON String: simple" $ expected @=? actual,
    do
        let actual = show $ String ""
        let expected ="\"\""
        testCase "Show JSON String: empty" $ expected @=? actual,
    do
        let actual = show $ String "J4HSKL\n4\never\n"
        let expected ="\"J4HSKL\\n4\\never\\n\""
        testCase "Show JSON String: newline" $ expected @=? actual,
    do
        let actual = show $ String "\\J4HSKL\\"
        let expected ="\"\\J4HSKL\\\""
        testCase "Show JSON String: backslash" $ expected @=? actual,
    do
        let actual = show $ String "\bTo\bto\b"
        let expected ="\"\\bTo\\bto\\b\""
        testCase "Show JSON String: backspace" $ expected @=? actual,
    do
        let actual = show $ String "\rTo\r to\n"
        let expected ="\"\\rTo\\r to\\n\""
        testCase "Show JSON String: carriage return" $ expected @=? actual,
    do
        let actual = show $ String "\fThis\nis a\f\ftest"
        let expected ="\"\\fThis\\nis a\\f\\ftest\""
        testCase "Show JSON String: formfeed" $ expected @=? actual,
    do
        let actual = show $ String $ printf "Hello%cWorld" (0xff :: Int)
        let expected ="\"Hello\\u00FFWorld\""
        testCase "Show JSON String: escaped hexadecimal (2-digits)" $ expected @=? actual,
    do
        let actual = show $ String $ printf "Hello%cWorld" (0x1 :: Int)
        let expected ="\"Hello\\u0001World\""
        testCase "Show JSON String: escaped hexadecimal (1-digit)" $ expected @=? actual,
    do
        let actual = show $ String $ printf "Good M%crning" (0x0 :: Int)
        let expected ="\"Good M\\u0000rning\""
        testCase "Show JSON String: escaped hexadecimal (zeros)" $ expected @=? actual,
    do
        let actual = show $ String $ printf "Hello%cWorld" (0xA2ea :: Int)
        let expected ="\"Hello\\uA2EAWorld\""
        testCase "Show JSON String: escaped hexadecimal (full-digit)" $ expected @=? actual,
    do
        let actual = show $ String $ printf "J%c4HSKL" (0x89AC :: Int)
        let expected ="\"J\\u89AC4HSKL\""
        testCase "Show JSON String: escaped hexadecimal followed by digit" $ expected @=? actual,
    do
        let actual = show $ String "J\"4HS\"KL"
        let expected ="\"J\\\"4HS\\\"KL\""
        testCase "Show JSON String: escaped quote" $ expected @=? actual,
    do
        let actual = show $ String "\nJ/4HS/KL"
        let expected ="\"\\nJ\\/4HS\\/KL\""
        testCase "Show JSON String: escaped slash" $ expected @=? actual,
    do
        let actual = show $ Bool True
        let expected ="true"
        testCase "Show JSON Bool: true" $ expected @=? actual,
    do
        let actual = show $ Bool False
        let expected ="false"
        testCase "Show JSON Bool: false" $ expected @=? actual,
    do
        let actual = show Null
        let expected ="null"
        testCase "Show JSON Null" $ expected @=? actual
    ]