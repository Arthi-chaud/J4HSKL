module TestJ4HSKL.PrettyPrint where

import Test.HUnit (Assertion, assertEqual, Testable (test), (@=?), (~=?))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Control.Applicative ( Alternative, (<|>), empty )
import J4HSKL.PrettyPrint
import J4HSKL.Data

testSuite :: Test
testSuite = testGroup "JSON Pretty-print module" [
    do
        let actual = show $ String "Hello World"
        let expected ="\"Hello World\""
        testCase "Pretty-print JSON String" $ expected @=? actual,
    do
        let actual = show $ Array []
        let expected ="[]"
        testCase "Pretty-print JSON Empty Array" $ expected @=? actual,
    do
        let actual = show $ Object []
        let expected ="{}"
        testCase "Pretty-print JSON Empty Object" $ expected @=? actual,
    do
        let actual = show $ Object [Pair("Hello", String "World")]
        let expected ="{\n\t\"Hello\": \"World\"\n}"
        testCase "Pretty-print JSON One-child Object" $ expected @=? actual,
    do
        let actual = show $ Array [String "Hello World"]
        let expected ="[\n\t\"Hello World\"\n]"
        testCase "Pretty-print JSON One-child Array" $ expected @=? actual,
    do
        let actual = show $ Array [String "Hello World"]
        let expected ="[\n\t\"Hello World\",\n\t123\n]"
        testCase "Pretty-print JSON Two-children Array" $ expected @=? actual,
    do
        let actual = show $ Object [Pair("Hello", Object[Pair("Key", String"World")])]
        let expected ="{\n\t\"Hello\": {\n\t\t\"Key\": \"World\"\n\t}\n}"
        testCase "Pretty-print JSON Nested Objects" $ expected @=? actual
    ]
