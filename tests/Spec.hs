import Test.Framework (defaultMain)
import TestBasicParser
import TestJ4HSKL.Parser
import TestJ4HSKL.Data
import Assets (loadTestAssets)

main :: IO ()
main = do
    testAssets <- Assets.loadTestAssets
    defaultMain [ 
        TestBasicParser.testSuite,
        TestJ4HSKL.Data.testSuite
        --TestJ4HSKL.Parser.testSuite testAssets 
        ]

    