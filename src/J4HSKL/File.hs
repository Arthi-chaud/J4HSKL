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
import Data.Maybe (isNothing)

-- | Export value to JSON-formatted in file
exportJSON :: JSONValue -> FilePath -> IO()
exportJSON value path = writeFile "file.txt" $ show value

-- | Dump data to JSON-formatted string, with indentation
dumpJSON :: JSONValue -> String
dumpJSON (Array a) = dumpJSONCollection '[' ']' 0 a
dumpJSON (Object a) = dumpJSONCollection '{' '}' 0 a
dumpJSON a = show a

dumpJSONCollection :: Char -> Char -> Int -> [JSONValue] -> String
dumpJSONCollection begin end _ [] = [begin, end]
dumpJSONCollection begin end depth a = printf "%c\n%s%s%c" begin content endIndent end
    where
        content = dumpJSONCollectionContent begin end (depth + 1) a
        endIndent = replicate depth '\t'

dumpJSONCollectionContent :: Char -> Char -> Int -> [JSONValue] -> String
dumpJSONCollectionContent _ _ _ [] = ""
dumpJSONCollectionContent begin end depth (head:rest) = indent ++ dumpedKey ++ dumpedValue ++ separator ++ dumpedRest
    where
        indent = replicate depth '\t'
        dumpedKey = case key of
            Nothing -> ""
            Just k -> printf "%s: " (dumpJSON $ String k)
        dumpedValue = case value of
            Array a -> dumpJSONCollection '[' ']' depth a
            Object a -> dumpJSONCollection '{' '}' depth a
            a -> show a
        separator = if null rest then "\n" else ",\n"
        dumpedRest = dumpJSONCollectionContent begin end depth rest
        (key, value) = case head of
            Pair (a, b) -> (Just a, b)
            a -> (Nothing, a)


-- | Import a file's content and parse its JSON content
importJSON :: FilePath -> IO JSONValue
importJSON filepath = do
    content <- readFile filepath
    case runParser parseJSON content of
        Just (jsonvalue, _) -> return jsonvalue
        _ -> error $ printf "%s: Invalid JSON content" filepath