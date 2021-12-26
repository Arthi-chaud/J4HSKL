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
import Data.Char (readLitChar, isHexDigit, digitToInt, chr)
import Data.List (nub)

-- | Parse Values fron JSON Data
parseJSON :: Parser JSONValue
parseJSON = parseJSONObject
        <|> parseJSONArray
        <|> parseJSONString
        <|> parseJSONNumber
        <|> parseJSONBool
        <|> parseJSONNull

-- | Same as 'parseJSON' but returns 'Nothing' if unparsed content
parseStrictJSON :: String -> Maybe JSONValue 
parseStrictJSON stream = do
    (res, rest) <- runParser parseJSON stream
    if null rest then return res else Nothing

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
            else return (Number $ number ^^ fromInteger exponent, rest2)
    where
        parseNumber = parseMinusZero <^> (parseLeadingZero <^> (parseFloat <|> (fromInteger <$> parseInt)))
        parseMinusZero = snd <$> parseChar '-' <&> parseLeadingZero
        parseLeadingZero = 0 <$ parseChar '0'
        parseSignAndLeadingZero = parseSign <&> parseLeadingZero
        parseExponent = parseAnyChar "eE" *> parseExponentNumber
        parseSign = parseAnyChar "-+" <|> pure '+'
        parseExponentNumber = uncurry applySign <$> (parseSign <&> ((round <$> parseLeadingZero) <^> parseUInt))
        applySign :: Char -> Integer -> Integer
        applySign '-' nb = nb * (-1)
        applySign _ nb = nb

-- | Parse String from JSON Data
parseJSONString :: Parser JSONValue
parseJSONString = String <$> (parseQuote *> parseJSONStringContent)
    where
        parseQuote = parseChar '\"'
        parseJSONStringContent :: Parser String
        parseJSONStringContent = ("" <$ parseQuote) <|> Parser (\s -> do
            (char, rest) <- runParser parseHead s
            case char of
                '\\' -> do
                    (litChar, rest2) <- runParser parseEscaped rest
                    runParser ((litChar:) <$> parseJSONStringContent) rest2
                _ -> runParser ((char:) <$> parseJSONStringContent) rest
            )
        parseEscaped = parseEscapedChar <|> (chr <$> parseEscapedAsciiCode)
        parseEscapedChar = parseChar '/' <|> ((\c -> fst $ head $ readLitChar ('\\' :  [c])) <$> parseAnyChar "bfnrt\\\"")
        parseEscapedAsciiCode = getNbfromHex <$> (parseChar 'u' *> parseN 4 (parseIf isHexDigit))
        getNbfromHex = foldl (\b c -> b * 16 + digitToInt c) 0

-- | Parse Object from JSON Data. Returns 'Nothing' of parsed object has duplicate keys
parseJSONObject :: Parser JSONValue
parseJSONObject = Parser $ \s -> do
    (pairs, rest) <- runParser (parseJSONCollection '{' '}' parseJSONPair) s 
    if nub (map keyList pairs) == map keyList pairs
        then return (Object pairs, rest)
        else Nothing
    where keyList = \(Pair (key, value)) -> key

-- | Parse pair in object from JSON
parseJSONPair :: Parser JSONPair
parseJSONPair = (\(String key, value) -> Pair (key, value)) <$> ((parseJSONString <* parseSeparator) <&> parseJSON)
    where
        parseSeparator = parseWhitespaces <* parseChar ':' <* parseWhitespaces

-- | Parse Array from JSON
parseJSONArray :: Parser JSONValue
parseJSONArray = Array <$> parseJSONCollection '[' ']' parseJSON

-- | Parse collection (array or object) from JSON using delimiters and a value parser
parseJSONCollection :: Char -> Char -> Parser a -> Parser [a]
parseJSONCollection begin end parseValue = parseEmptyCollection <|> (parseChar begin *> parseCollectionContent)
    where
        parseEmptyCollection = [] <$ parseChar begin <* parseWhitespaces <&> parseChar end
        parseCollectionContent = Parser $ \s -> do
            ((value, next), rest) <- runParser ((parseWhitespaces *> parseValue <* parseWhitespaces) <&> parseHead) s
            if next == end then return ([value], rest)
            else case next of
                ',' -> runParser ((value:) <$> parseCollectionContent) rest
                _ -> Nothing
