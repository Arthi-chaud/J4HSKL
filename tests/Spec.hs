import Test.Framework (defaultMain)
import TestBasicParser
import TestJ4HSKL.Parser
import System.Directory
import Debug.Trace

main :: IO ()
main = do
    assetsFolders <- getDirectoryContents "tests/assets"
    assetsFolders = filter (\name -> head name /= '.') assetsFolders
    defaultMain [ TestBasicParser.testSuite ]

    