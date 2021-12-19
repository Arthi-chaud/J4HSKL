import TestParser (testSuite)
import Test.HUnit (Assertion, assertEqual, Testable (test))
import Test.Framework (Test, testGroup, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
main :: IO ()
main = defaultMain
    [ TestParser.testSuite ]

-- specs :: Test
-- specs = testGroup "Campaign-related functions"
--     [
--         testCase "Parser: case_parseChar_example_1" TestParser.case_parseChar_example_1,
--         testCase "Parser: case_parseChar_example_2" TestParser.case_parseChar_example_2,
--         testCase "Parser: case_parseChar_example_3" TestParser.case_parseChar_example_3,
--         testCase "Parser: case_parseChar_example_4" TestParser.case_parseChar_example_4,
--         testCase "Parser: case_parseAnyChar_example_1" TestParser.case_parseAnyChar_example_1,
--         testCase "Parser: case_parseAnyChar_example_2" TestParser.case_parseAnyChar_example_2,
--         testCase "Parser: case_parseAnyChar_example_3" TestParser.case_parseAnyChar_example_3,
--         testCase "Parser: case_parseOr_example_1" TestParser.case_parseOr_example_1,
--         testCase "Parser: case_parseOr_example_2" TestParser.case_parseOr_example_2,
--         testCase "Parser: case_parseOr_example_3" TestParser.case_parseOr_example_3,
--         testCase "Parser: case_parseAnd_example_1" TestParser.case_parseAnd_example_1,
--         testCase "Parser: case_parseAnd_example_2" TestParser.case_parseAnd_example_2,
--         testCase "Parser: case_parseAnd_example_3" TestParser.case_parseAnd_example_3,
--         testCase "Parser: case_parseAndWith_example_1" TestParser.case_parseAndWith_example_1,
--         testCase "Parser: case_parseMany_example_1" TestParser.case_parseMany_example_1,
--         testCase "Parser: case_parseMany_example_2" TestParser.case_parseMany_example_2,
--         testCase "Parser: case_parseSome_example_1" TestParser.case_parseSome_example_1,
--         testCase "Parser: case_parseSome_example_2" TestParser.case_parseSome_example_2,
--         testCase "Parser: case_parseInt_example" TestParser.case_parseInt_example,
--         testCase "Parser: case_parseWord" TestParser.case_parseWord,
--         testCase "Parser: case_parseWord_frontSpace," TestParser.case_parseWord_frontSpace
--     ]