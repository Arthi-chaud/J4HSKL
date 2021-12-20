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

-- | Defines types of JSON Values
data JSONValue = Null
               | Bool JSONBool
               | Number JSONNumber
               | String JSONString
               | Array JSONArray
               | Object JSONObject

-- | Boolean for JSON
type JSONBool = Bool

-- | Number for JSON
data JSONNumber = Integer

-- | String for JSON
type JSONString = String 

-- | Array for JSON
type JSONArray = [JSONValue]

-- | Pair for JSON (only used in object, never as standalone)
type JSONPair = (JSONString, JSONValue)

-- | JSON's Object
type JSONObject = [JSONPair]