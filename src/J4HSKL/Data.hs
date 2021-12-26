{-|
Module      : J4HSL.Data
Description : Data representation for JSON values
Copyright   : (c) Arthui-chaud, 2021
License     : GPL-3
Maintainer  : arthur.jamet@gmail.com
Stability   : experimental
Portability : POSIX

-}
module J4HSKL.Data where


import Text.Printf(printf)
import Data.List (intercalate)
import Data.Char (isSpace, showLitChar)
import BasicParser
import Numeric (showGFloatAlt)

-- | Defines types of JSON Values
data JSONValue = 
    Null -- ^ 
   | Bool Bool -- ^ Boolean for JSON
   | Number Float  -- ^ Number for JSON
   | String String -- ^ String for JSON
   | Array [JSONValue] -- ^ Array for JSON
   | Pair (String, JSONValue) -- ^ Pair for Objects (only used in object, never as standalone)
   | Object [JSONValue] -- ^ JSON's Object


instance Show JSONValue where
    show Null = "null"
    show (Bool v) | v = "true"
                  | otherwise = "false"
    show (Number v) = showJSONNumber v
    show (String v) = printf "\"%s\"" $ showJSONString v
    show (Array v) = printf "[%s]" (intercalate ", " $ map show v)
    show (Pair (key, value)) = printf "%s: %s" (show key) $ show value
    show (Object v) = printf "{%s}" (intercalate ", " $ map show v)

instance Eq JSONValue where
    (==) Null Null = True
    (==) (Bool a) (Bool b) = a == b
    (==) (Number a) (Number b) = a == b
    (==) (String a) (String b) = a == b
    (==) (Array a) (Array b) = a == b
    (==) (Object a) (Object b) = a == b
    (==) (Pair a) (Pair b) = a == b
    (==) _ _ = False

-- | Fromat String for JSON (without quotes)
showJSONString :: String -> String
showJSONString "" = ""
showJSONString (char:rest) =  case runParser (parseAnyChar "/\\\"") (char:rest) of
    Just (c, rest) -> printf "\\%c%s" c $ showJSONString rest
    Nothing -> if length litChar <= 2
    then litChar ++ showJSONString rest
    else (printf "\\u%04X" char) ++ showJSONString rest
    where
        litChar = showLitChar char ""

-- | Format Number for JSON
showJSONNumber :: Float -> String 
showJSONNumber number = case drop (strlen - 2) string of
    ".0" -> take (strlen - 2) string
    _ -> string
    where
        strlen = length string
        string = showGFloatAlt Nothing number ""