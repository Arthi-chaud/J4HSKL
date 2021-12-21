import Test.Framework (defaultMain)
import TestBasicParser
import TestJ4HSKL.Parser
import Debug.Trace
import Assets (loadTestAssets)

main :: IO ()
main = do
    defaultMain [ TestBasicParser.testSuite ]

    