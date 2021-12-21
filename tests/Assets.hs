module Assets where

type FileContent = String
type TestAsset = (FilePath, FileContent)
type TestAssets = ([TestAsset], [TestAsset])

-- loadTestAssets :: IO TestAssets
-- loadTestAssets = do
--     let assetsDirectory = "tests/assets/"
--     let validAssetsDirectory = assetsDirectory ++ "valid"
--     let invalidAssetsDirectory = assetsDirectory ++ "invalid"
-- 	let validAssets = getDirectoryContents validAssetsDirectory

loadTestAssetsFromFolder :: FilePath -> IO [TestAsset]
loadTestAssetsFromFolder assetsFolder = do
    assetsFiles <- getDirectoryContents assetsFolder
	map (\path -> assetsFiles

loadTestAssetsFromFile