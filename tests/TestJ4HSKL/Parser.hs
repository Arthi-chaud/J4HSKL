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
        testInvalidAsset (assetName, asset) = testCase (getTestName assetName) $ Nothing @=? (runParser parseJSON asset)
        
        testValidAsset :: [TestAsset] -> String -> Maybe (JSONValue, String) -> Test
        testValidAsset assets assetName expected = case find (\s -> (takeBaseName (fst s)) == assetName) assets of
            Nothing -> testCase (getTestName assetName) $ assertFailure (printf "%s: No such asset" assetName) 
            Just (name, content) -> 
                testCase (getTestName name) $ expected @=? (runParser parseJSON content)
        
        testsOnInvalid = map testInvalidAsset invalidAssets
        
        testsOnValid = [
            testValidAsset validAssets "array_empty" $ Just (Array [], ""),
            testValidAsset validAssets "array_mulitple_values" $ Just (Array [String "Hello", String "World", String "J4HSKL"], ""),
            testValidAsset validAssets "array_multiple_values_mixed" $ Just (Null, ""),
            testValidAsset validAssets "array_one_value" $ Just (Null, ""),
            testValidAsset validAssets "false" $ Just (Null, ""),
            testValidAsset validAssets "nested" $ Just (Null, ""),
            testValidAsset validAssets "null" $ Just (Null, ""),
            testValidAsset validAssets "number_exposed" $ Just (Null, ""),
            testValidAsset validAssets "number_exposed_negative" $ Just (Null, ""),
            testValidAsset validAssets "number_exposed_positive" $ Just (Null, ""),
            testValidAsset validAssets "number_exposed_uppercase" $ Just (Null, ""),
            testValidAsset validAssets "number_float_exposed" $ Just (Null, ""),
            testValidAsset validAssets "number_float_exposed_negative" $ Just (Null, ""),
            testValidAsset validAssets "number_float" $ Just (Null, ""),
            testValidAsset validAssets "number" $ Just (Null, ""),
            testValidAsset validAssets "number_negative" $ Just (Null, ""),
            testValidAsset validAssets "number_negative_zero" $ Just (Null, ""),
            testValidAsset validAssets "object_curly_bracket_in_key" $ Just (Null, ""),
            testValidAsset validAssets "object_curly_bracket_in_value" $ Just (Null, ""),
            testValidAsset validAssets "object_empty" $ Just (Null, ""),
            testValidAsset validAssets "object_multiple_pairs" $ Just (Null, ""),
            testValidAsset validAssets "object_multiple_pairs_mixed" $ Just (Null, ""),
            testValidAsset validAssets "object_one_pair" $ Just (Null, ""),
            testValidAsset validAssets "string_empty" $ Just (Null, ""),
            testValidAsset validAssets "string_escaped_backslash" $ Just (Null, ""),
            testValidAsset validAssets "string_escaped_backspace" $ Just (Null, ""),
            testValidAsset validAssets "string_escaped_carriage_return" $ Just (Null, ""),
            testValidAsset validAssets "string_escaped_formfeed" $ Just (Null, ""),
            testValidAsset validAssets "string_escaped_hex_five" $ Just (Null, ""),
            testValidAsset validAssets "string_escaped_hex" $ Just (Null, ""),
            testValidAsset validAssets "string_escaped_hex_zeros" $ Just (Null, ""),
            testValidAsset validAssets "string_escaped_newline" $ Just (Null, ""),
            testValidAsset validAssets "string_escaped_quote" $ Just (Null, ""),
            testValidAsset validAssets "string_escaped_slash" $ Just (Null, ""),
            testValidAsset validAssets "string" $ Just (Null, ""),
            testValidAsset validAssets "true" $ Just (Null, "")
            ]