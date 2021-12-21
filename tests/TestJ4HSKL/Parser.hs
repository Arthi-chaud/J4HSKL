module TestJ4HSKL.Parser where

import Assets
import Test.HUnit (Assertion, assertEqual, Testable (test), (@=?), (~=?))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import BasicParser (Parser(runParser))
import J4HSKL.Parser (parseJSON)
import Text.Printf (printf)
import System.FilePath (takeBaseName)

testSuite :: TestAssets -> Test
testSuite (validAssets, invalidAssets) = testGroup "JSON Parser module" testsOnInvalid
    where
        testsOnInvalid = map testInvalidAsset invalidAssets
    
        testInvalidAsset :: TestAsset -> Test
        testInvalidAsset (assetName, asset) = do
            let actual = runParser parseJSON asset 
            let expected = Nothing
            let formattedAssetName = map (\c -> if c == '_' then ' ' else c) (takeBaseName assetName)
            let testName = printf "parseJSON: %s" formattedAssetName
            testCase testName $ expected @=? actual