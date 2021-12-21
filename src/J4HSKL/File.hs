{-|
Module      : J4HSL.File
Description : Interact with Files
Copyright   : (c) Arthui-chaud, 2021
License     : GPL-3
Maintainer  : arthur.jamet@gmail.com
Stability   : experimental
Portability : POSIX

-}
module J4HSKL.File where

import J4HSKL.Data
import J4HSKL.Parser
import BasicParser (Parser(runParser))
import GHC.Base (error)
import Text.Printf(printf) 
    
exportJSON :: JSONValue -> FilePath -> IO()
exportJSON value path = writeFile "file.txt" $ show value

-- | Import a file's content and parse its JSON content
importJSON :: FilePath -> IO JSONValue 
importJSON filepath = do
    content <- readFile filepath
    case runParser parseJSON content of
        Just (jsonvalue, _) -> return jsonvalue
        _ -> error $ printf "%s: Invalid JSON content" filepath 