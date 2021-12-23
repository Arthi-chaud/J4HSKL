{-|
Module      : J4HSL.PArser
Description : Parser for JSON Data
Copyright   : (c) Arthui-chaud, 2021
License     : GPL-3
Maintainer  : arthur.jamet@gmail.com
Stability   : experimental
Portability : POSIX

-}
module J4HSKL.Parser where

import J4HSKL.Data
import BasicParser
import Text.Read (readMaybe)
import Control.Applicative

parseJSON :: Parser JSONValue
parseJSON = parseJSONNumber <|> parseJSONNull <|> parseJSONBool

-- | Parse null from JSON Data
parseJSONNull :: Parser JSONValue
parseJSONNull = Parser $ \s -> do
    (word, rest) <- runParser parseWord s
    if word == "null" then return (Null, rest)
    else Nothing

-- | Parse boolean from JSON Data
parseJSONBool :: Parser JSONValue
parseJSONBool = Parser $ \s -> do
    (word, rest) <- runParser parseWord s
    case word of
        "true" -> return (Bool True, rest)
        "false" -> return (Bool False, rest)
        _ -> Nothing

-- | Parse Number from JSON Data
parseJSONNumber :: Parser JSONValue
parseJSONNumber = Parser $ \s -> do
    (number, rest) <- runParser parseFloat s
    case runParser parseExponentString rest of
        Nothing -> Just (Number number, rest)
        Just (exponent, rest2) -> do
            exposedNumber <- readMaybe (show number ++ exponent) :: Maybe Float
            return (Number exposedNumber, rest2)
    where
        parseExponentString = uncurry (:) <$> (parseAnyChar "eE" <&> parseExponentNumberString)
        parseExponentNumberString = uncurry (:) <$> (parseIf (`elem` "-+") <&> (show <$> parseInt))
