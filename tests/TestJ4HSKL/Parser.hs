module TestJ4HSKL.Parser where

import Assets
import Test.HUnit (Assertion, assertEqual, assertFailure, Testable (test), (@=?))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import BasicParser (Parser(runParser))
import J4HSKL.Parser (parseStrictJSON, parseJSON)
import J4HSKL.Data
import Text.Printf (printf)
import System.FilePath (takeBaseName)
import Data.List (find)

testSuite :: TestAssets -> Test
testSuite (validAssets, invalidAssets) = testGroup "JSON Parser module" (testsOnInvalid ++ testsOnValid)
    where
        getTestName :: FilePath -> String
        getTestName assetPath = printf "parseJSON: %s" (map (\c -> if c == '_' then ' ' else c) (takeBaseName assetPath)) 
        
        testInvalidAsset :: TestAsset -> Test
        testInvalidAsset (assetName, asset) = testCase (getTestName assetName) $ Nothing @=? parseStrictJSON asset
        
        testValidAsset :: [TestAsset] -> String -> JSONValue -> Test
        testValidAsset assets assetName expected = case find (\s -> takeBaseName (fst s) == assetName) assets of
            Nothing -> testCase (getTestName assetName) $ assertFailure (printf "%s: No such asset" assetName) 
            Just(name, content) -> case runParser parseJSON content of
                Just (actual, "") -> testCase (getTestName name) $ expected @=? actual
                res -> testCase (getTestName assetName) $ assertFailure $ printf "Parsing failed: %s" (show res)
               
        
        testsOnInvalid = map testInvalidAsset invalidAssets
        
        testsOnValid = [
            testValidAsset validAssets "array_empty" (Array []),
            testValidAsset validAssets "array_mulitple_values" (Array [String "Hello", String "World", String "J4HSKL"]),
            testValidAsset validAssets "array_multiple_values_mixed" (Array [String "Hello", Number 1, String "J4HSKL", Bool True, Null]),
            testValidAsset validAssets "array_one_value" (Array [String "Hello World"]),
            testValidAsset validAssets "false" (Bool False),
            testValidAsset validAssets "null" Null,
            testValidAsset validAssets "number_exposed" (Number (3 ^ 2)),
            testValidAsset validAssets "number_exposed_negative" (Number (3 ^^(-10))),
            testValidAsset validAssets "number_exposed_positive" (Number (30 ^ 2)),
            testValidAsset validAssets "number_exposed_uppercase" (Number (10 ^ 34)),
            testValidAsset validAssets "number_float_exposed" (Number (3 ^ 2)),
            testValidAsset validAssets "number_float_exposed_negative" (Number (123.45678 ** (-10))),
            testValidAsset validAssets "number_float" (Number 123.45678),
            testValidAsset validAssets "number" (Number 3),
            testValidAsset validAssets "number_negative" (Number(-123)),
            testValidAsset validAssets "number_negative_zero" (Number 0),
            testValidAsset validAssets "object_curly_bracket_in_key" (Object [Pair ("Key{", String "Value")]),
            testValidAsset validAssets "object_curly_bracket_in_value" (Object [Pair ("Key", String "Value}")]),
            testValidAsset validAssets "object_empty" (Object []),
            testValidAsset validAssets "object_multiple_pairs" (Object [
                                                                                Pair ("Key1", String "Value1"),
                                                                                Pair ("Key2", String "Value2"),
                                                                                Pair ("Key3", String "Value3"),
                                                                                Pair ("Key4", String "Value4"),
                                                                                Pair ("Key5", String "Value5")
                                                                                ]),
            testValidAsset validAssets "object_multiple_pairs_mixed" (Object [
                                                                                Pair ("Key1", String "Value1"),
                                                                                Pair ("Number", Number 2),
                                                                                Pair ("Key3", String "Value3"),
                                                                                Pair ("Bool", Bool True),
                                                                                Pair ("Bool1", Bool False),
                                                                                Pair ("Null", Null)
                                                                                ]),
            testValidAsset validAssets "object_one_pair" (Object [Pair ("Key", String "Value")]),
            testValidAsset validAssets "string_empty" (String ""),
            testValidAsset validAssets "string_escaped_backslash" (String "Hello\\"),
            testValidAsset validAssets "string_escaped_backspace" (String "\bWorld"),
            testValidAsset validAssets "string_escaped_carriage_return" (String "Hello\rWorld"),
            testValidAsset validAssets "string_escaped_formfeed" (String "123\f123"),
            testValidAsset validAssets "string_escaped_hex_five" (String (printf "Hello%cEWorld" (0xabcd :: Int))),
            testValidAsset validAssets "string_escaped_hex" (String (printf "Hello%cWorld" (0xA2ae :: Int))),
            testValidAsset validAssets "string_escaped_hex_zeros" (String (printf "Hello%cWorld" (0x0000 :: Int))),
            testValidAsset validAssets "string_escaped_newline" (String "Hello\nWorld"),
            testValidAsset validAssets "string_escaped_quote" (String "Hello\"World"),
            testValidAsset validAssets "string_escaped_slash" (String "Hello/World"),
            testValidAsset validAssets "string" (String "Hello World"),
            testValidAsset validAssets "true" (Bool True),
            testValidAsset validAssets "nested" (Object [
                Pair("info", Object [
                    Pair ("_postman_id", String "my_id"),
                    Pair ("name", String "project"),
                    Pair ("schema", String "https://schema.getpostman.com/json/collection/v2.1.0/collection.json")
                ]),
                Pair("item", Object [
                    Pair ("name", String "Auth")
                ]),
                Pair("variable", Array [
                    Object [
                        Pair ("key", String "accessToken"),
                        Pair ("value", String "\"\"")
                    ]
                ])
            ])
            ]