module TestJ4HSKL.Parser where

import Assets
import Test.HUnit (Assertion, assertEqual, assertFailure, Testable (test), (@=?))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import BasicParser (Parser(runParser))
import J4HSKL.Parser (parseJSON)
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
        testInvalidAsset (assetName, asset) = testCase (getTestName assetName) $ Nothing @=? runParser parseJSON asset
        
        testValidAsset :: [TestAsset] -> String -> Maybe (JSONValue, String) -> Test
        testValidAsset assets assetName expected = case find (\s -> takeBaseName (fst s) == assetName) assets of
            Nothing -> testCase (getTestName assetName) $ assertFailure (printf "%s: No such asset" assetName) 
            Just (name, content) -> 
                testCase (getTestName name) $ expected @=? runParser parseJSON content
        
        testsOnInvalid = map testInvalidAsset invalidAssets
        
        testsOnValid = [
            testValidAsset validAssets "array_empty" $ Just (Array [], ""),
            testValidAsset validAssets "array_mulitple_values" $ Just (Array [String "Hello", String "World", String "J4HSKL"], ""),
            testValidAsset validAssets "array_multiple_values_mixed" $ Just (Array [String "Hello", Number 1, String "J4HSKL", Bool True, Null],  ""),
            testValidAsset validAssets "array_one_value" $ Just (Array [String "Hello World"], ""),
            testValidAsset validAssets "false" $ Just (Bool False, ""),
            testValidAsset validAssets "null" $ Just (Null, ""),
            testValidAsset validAssets "number_exposed" $ Just (Number (3 ^ 2), ""),
            testValidAsset validAssets "number_exposed_negative" $ Just (Number (3 ^(-10)), ""),
            testValidAsset validAssets "number_exposed_positive" $ Just (Number (30 ^ 2), ""),
            testValidAsset validAssets "number_exposed_uppercase" $ Just (Number (10 ^ 34), ""),
            testValidAsset validAssets "number_float_exposed" $ Just (Number (3 ^ 2), ""),
            testValidAsset validAssets "number_float_exposed_negative" $ Just (Number (123.45678 ** (-10)), ""),
            testValidAsset validAssets "number_float" $ Just (Number 123.45678, ""),
            testValidAsset validAssets "number" $ Just (Number 3, ""),
            testValidAsset validAssets "number_negative" $ Just (Number(-123), ""),
            testValidAsset validAssets "number_negative_zero" $ Just (Number 0, ""),
            testValidAsset validAssets "object_curly_bracket_in_key" $ Just (Object [Pair ("Key{", String "Value")], ""),
            testValidAsset validAssets "object_curly_bracket_in_value" $ Just (Object [Pair ("Key", String "Value}")], ""),
            testValidAsset validAssets "object_empty" $ Just (Object [], ""),
            testValidAsset validAssets "object_multiple_pairs" $ Just (Object [
                                                                                Pair ("Key1", String "Value1"),
                                                                                Pair ("Key2", String "Value2"),
                                                                                Pair ("Key3", String "Value3"),
                                                                                Pair ("Key4", String "Value4"),
                                                                                Pair ("Key5", String "Value5")
                                                                                ], ""),
            testValidAsset validAssets "object_multiple_pairs_mixed" $ Just (Object [
                                                                                Pair ("Key1", String "Value1"),
                                                                                Pair ("Number", Number 2),
                                                                                Pair ("Key3", String "Value3"),
                                                                                Pair ("Bool", Bool True),
                                                                                Pair ("Bool1", Bool False),
                                                                                Pair ("Null", Null)
                                                                                ], ""),
            testValidAsset validAssets "object_one_pair" $ Just (Object [Pair ("Key", String "Value")], ""),
            testValidAsset validAssets "string_empty" $ Just (String "", ""),
            testValidAsset validAssets "string_escaped_backslash" $ Just (String "Hello\\", ""),
            testValidAsset validAssets "string_escaped_backspace" $ Just (String "\bWorld", ""),
            testValidAsset validAssets "string_escaped_carriage_return" $ Just (String "Hello\rWorld", ""),
            testValidAsset validAssets "string_escaped_formfeed" $ Just (String "123\f123", ""),
            testValidAsset validAssets "string_escaped_hex_five" $ Just (String (printf "Hello%cEWorld" (0xabcd :: Int)), ""),
            testValidAsset validAssets "string_escaped_hex" $ Just (String (printf "Hello%cWorld" (0xA2ae :: Int)), ""),
            testValidAsset validAssets "string_escaped_hex_zeros" $ Just (String (printf "Hello%cWorld" (0x0000 :: Int)), ""),
            testValidAsset validAssets "string_escaped_newline" $ Just (String "Hello\nWorld", ""),
            testValidAsset validAssets "string_escaped_quote" $ Just (String "Hello\"World", ""),
            testValidAsset validAssets "string_escaped_slash" $ Just (String "Hello/World", ""),
            testValidAsset validAssets "string" $ Just (String "Hello World", ""),
            testValidAsset validAssets "true" $ Just (Bool True, ""),
            testValidAsset validAssets "nested" $ Just (Null, "")
            ]