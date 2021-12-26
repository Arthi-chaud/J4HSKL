module TestJ4HSKL.File where

import Test.HUnit (Assertion, assertEqual, Testable (test), (@=?), (~=?))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Control.Applicative ( Alternative, (<|>), empty )
import J4HSKL.File(dumpJSON)
import J4HSKL.Data

testSuite :: Test
testSuite = testGroup "JSON-File management module" [
    do
        let actual = dumpJSON $ String "Hello World"
        let expected ="\"Hello World\""
        testCase "Dump JSON String" $ expected @=? actual,
    do
        let actual = dumpJSON $ Array []
        let expected ="[]"
        testCase "Dump JSON Empty Array" $ expected @=? actual,
    do
        let actual = dumpJSON $ Object []
        let expected ="{}"
        testCase "Dump JSON Empty Object" $ expected @=? actual,
    do
        let actual = dumpJSON $ Object [Pair("Hello", String "World")]
        let expected ="{\n\t\"Hello\": \"World\"\n}"
        testCase "Dump JSON One-child Object" $ expected @=? actual,
    do
        let actual = dumpJSON $ Array [String "Hello World"]
        let expected ="[\n\t\"Hello World\"\n]"
        testCase "Dump JSON One-child Array" $ expected @=? actual,
    do
        let actual = dumpJSON $ Array [String "Hello World", Number 123]
        let expected ="[\n\t\"Hello World\",\n\t123\n]"
        testCase "Dump JSON Two-children Array" $ expected @=? actual,
    do
        let actual = dumpJSON $ Object [Pair("Hello", Object[Pair("Key", String "World")])]
        let expected ="{\n\t\"Hello\": {\n\t\t\"Key\": \"World\"\n\t}\n}"
        testCase "Dump JSON Nested Objects" $ expected @=? actual,
    do
        let actual = dumpJSON $ Array [Number 1, Array [Number 2], Number 3]
        let expected ="[\n\t1,\n\t[\n\t\t2\n\t],\n\t3\n]"
        testCase "Dump JSON Nested Arrays" $ expected @=? actual
    ]
