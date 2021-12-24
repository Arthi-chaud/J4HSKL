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
parseJSON = parseJSONNumber
        <|> parseJSONNull
        <|> parseJSONBool

-- | Parse null from JSON Data
parseJSONNull :: Parser JSONValue
parseJSONNull = Null <$ parseString "null"

-- | Parse boolean from JSON Data
parseJSONBool :: Parser JSONValue
parseJSONBool = (Bool False <$ parseString "false") <|> (Bool True <$ parseString "true")

-- | Parse Number from JSON Data
parseJSONNumber :: Parser JSONValue
parseJSONNumber = Parser $ \s -> do
    (number, rest) <- runParser parseNumber  s
    if not (null rest) && head rest == '.'
    then Nothing
    else case runParser parseExponent rest of
        Nothing -> Just (Number number, rest)
        Just (exponent, rest2) -> if not (null rest2) && head rest2 == '.'
            then Nothing
            else return (Number $ fromInteger exponent, rest2)
    where
        parseNumber = parseLeadingZero <^> (parseFloat <|> (fromInteger <$> parseInt))
        parseLeadingZero = 0 <$ parseChar '0'
        parseExponent = parseAnyChar "eE" *> parseExponentNumber
        parseExponentSign = parseAnyChar "-+" <|> pure '+'
        parseExponentNumber = uncurry applySign <$> (parseExponentSign <&> ((round <$> parseLeadingZero) <^> parseUInt))
        applySign :: Char -> Integer -> Integer
        applySign '-' nb = nb * (-1)
        applySign _ nb = nb
