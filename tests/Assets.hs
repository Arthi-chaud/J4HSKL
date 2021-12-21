module Assets where

import System.Directory(getDirectoryContents)

type FileContent = String
type TestAsset = (FilePath, FileContent)
type TestAssets = ([TestAsset], [TestAsset])

loadTestAssets :: IO TestAssets
loadTestAssets = do
    let assetsDirectory = "tests/assets/"
    let validAssetsDirectory = assetsDirectory ++ "valid/"
    let invalidAssetsDirectory = assetsDirectory ++ "invalid/"
    validAssets <- loadTestAssetsFromFolder validAssetsDirectory
    invalidAssets <- loadTestAssetsFromFolder invalidAssetsDirectory
    return (validAssets, invalidAssets)

loadTestAssetsFromFolder :: FilePath -> IO [TestAsset]
loadTestAssetsFromFolder assetsFolder = do
    directoryContent <- getDirectoryContents assetsFolder
    let assetsFiles = filter (\name -> head name /= '.') directoryContent
    loadTestAssetsFromFiles (map (assetsFolder ++) assetsFiles)

loadTestAssetsFromFiles :: [FilePath] -> IO [TestAsset]
loadTestAssetsFromFiles [] = return []
loadTestAssetsFromFiles (filePath:rest) = do
    asset <- loadTestAssetFromFile filePath
    otherAssets <- loadTestAssetsFromFiles rest
    return (asset : otherAssets)

loadTestAssetFromFile :: FilePath -> IO TestAsset
loadTestAssetFromFile filepath = do
    content <- readFile filepath
    return (filepath, content)