import Test.Framework (defaultMain)
import TestBasicParser
import TestJ4HSKL.Parser
import Debug.Trace
import Assets (loadTestAssets)

main :: IO ()
main = do
    testAssets <- Assets.loadTestAssets 
    traceIO $ show (map (\asset-> fst asset) (fst testAssets))
    defaultMain [ TestBasicParser.testSuite, TestJ4HSKL.Parser.testSuite testAssets ]

    