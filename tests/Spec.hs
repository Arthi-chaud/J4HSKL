import TestBasicParser (testSuite)
import Test.HUnit (Assertion, assertEqual, Testable (test))
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
main :: IO ()
main = defaultMain
    [ TestBasicParser.testSuite ]
