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
        let actual = show $ Number 123
        let expected ="123"
        testCase "Show JSON Number: positive" $ expected @=? actual,
    do
        let actual = show $ Number 123.5
        let expected ="123.5"
        testCase "Show JSON Number: positive float" $ expected @=? actual,
    do
        let actual = show $ Number (-12)
        let expected ="123"
        testCase "Show JSON Number: negative" $ expected @=? actual,
    do
        let actual = show $ Number (-12.3)
        let expected ="12.3"
        testCase "Show JSON Number: negative float" $ expected @=? actual,
    do
        let actual = show $ Number (3 ^^(-10))
        let expected ="3e-10"
        testCase "Show JSON Number: Exposed to negative" $ expected @=? actual,
    do
        let actual = show $ Number (10 ^^ 3)
        let expected ="10e3"
        testCase "Show JSON Number: Exposed to positive" $ expected @=? actual,
    do
        let actual = show $ Number (2.3 ^^ 10)
        let expected ="2.3e10"
        testCase "Show JSON Number: Exposed Float" $ expected @=? actual,
    do
        let actual = show $ Number (1.3 ^^ (-2))
        let expected ="1.3e-2"
        testCase "Show JSON Number: Exposed Float to negative" $ expected @=? actual,
    do
        let actual = show $ Number (2 ** 2.3)
        let expected ="2e2"
        testCase "Show JSON Number: Exposed to Float (invalid JSON -> converts to integer)" $ expected @=? actual,
    do
        let actual = show $ Array []
        let expected ="[]"
        testCase "Show JSON Array: Empty" $ expected @=? actual,
    do
        let actual = show $ Array [String "Hello World"]
        let expected ="[\"Hello World\"]"
        testCase "Show JSON Array: One String" $ expected @=? actual,
    do
        let actual = show $ Array [Bool False ]
        let expected ="[false]"
        testCase "Show JSON Array: One Bool" $ expected @=? actual,
    do
        let actual = show $ Array [Number 1, Number 2, Number 3]
        let expected ="[1, 2, 3]"
        testCase "Show JSON Array: Three Elements" $ expected @=? actual,
    do
        let actual = show $ Array [Number 1, Number 2.3, Number 3]
        let expected ="[1, 2.3, 3]"
        testCase "Show JSON Array: Three Elements - mixed numbers " $ expected @=? actual,
    do
        let actual = show $ Array [Bool True, Number 2.3, String "J\"4\"HSKL", Null]
        let expected ="[true, 2.3, \"J\\\"4\\\"HSKL\", null]"
        testCase "Show JSON Array: Three Elements - mixed types " $ expected @=? actual,
    do
        let actual = show $ Object []
        let expected ="{}"
        testCase "Show JSON Object: Empty" $ expected @=? actual,
    do
        let actual = show $ Object [Pair ("Key", String "Value")]
        let expected ="{\"Key\": \"Value\"}"
        testCase "Show JSON Object: One Pair" $ expected @=? actual,
    do
        let actual = show $ Object [Pair ("Key1", String "Value1"), Pair ("Key2", Number 2)]
        let expected ="{\"Key1\": \"Value1\", \"Key2\": 2}"
        testCase "Show JSON Object: Two Pairs - mixed types" $ expected @=? actual,
    do
        let actual = show $ Object [Pair ("Key1", Object [Pair ("Nested Key", Null)]), Pair ("Key2", Number 2)]
        let expected ="{\"Key1\": {\"Nested Key\": null}, \"Key2\": 2}"
        testCase "Show JSON Object: Nested Object" $ expected @=? actual,
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