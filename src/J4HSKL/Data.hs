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

-- | Defines types of JSON Values
data JSONValue = Null
               | Bool JSONBool
               | Number JSONNumber
               | String JSONString
               | Array JSONArray
               | Object JSONObject


instance Show JSONValue where
    show Null = "null"
    show (Bool v) | v = "true"
                  | otherwise = "false"
    show (Number v) = show v
    show (String v) = printf "\"%s\"" v
    show (Array v) = printf "[%s]" (intercalate ", " $ map show v)
    show (Object v) = printf "{%s}" (intercalate ", " $ map show v)


-- | Boolean for JSON
type JSONBool = Bool

-- | Number for JSON
type JSONNumber = Integer

-- | String for JSON
type JSONString = String 

-- | Array for JSON
type JSONArray = [JSONValue]

-- | Pair for JSON (only used in object, never as standalone)
newtype JSONPair = Pair (JSONString, JSONValue)
instance Show JSONPair where
    show (Pair (key, value)) = printf "%s: %s" (show key) $ show value

-- | JSON's Object
type JSONObject = [JSONPair]