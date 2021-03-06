
{-|
Module      : Basic Parser
Description : Primitive Monadic Parser
Copyright   : (c) Arthui-chaud, 2021
License     : GPL-3
Maintainer  : arthur.jamet@gmail.com
Stability   : experimental
Portability : POSIX
-}

module BasicParser where

import Data.Maybe (isNothing)
import Text.Read
import Data.Char(isSpace)
import Control.Applicative ( Alternative, (<|>), empty, some)

-- | A 'ParserFunction' function take a string a parameter,
-- maybe returns a successfull value of type "a" tuppled with a string (the rest of the string)
type ParserFunction a = String -> Maybe (a, String)

-- | A 'Parser' of type "a" holds a 'ParserFunction' of same type.
data Parser a = Parser {
    runParser :: ParserFunction a
}

instance Functor Parser where
    -- | Creates a 'Parser' from parameter and applies 'fct' on parsed result on success
    fmap fct parser = Parser (\s -> do
        (parsed, rest) <- runParser parser s
        Just (fct parsed, rest)
        )

instance Applicative Parser where
    -- | Creates a 'Parser' which, on call, will always return 'a' and untouched 's'
    pure a = Parser (\s -> Just(a, s))

    -- | Applies function of 'fp' on result of 'p'
    (<*>) fp p = Parser (\string -> do
        (f, rest) <- runParser fp string
        runParser (f <$> p) rest
        )

instance Alternative Parser where
    -- | Creates a 'Parser' that always returns 'Nothing'
    empty = Parser(const Nothing)

    -- | If first parser fails, returns results of second parser
    -- On success of the first, its result is returned
    (<|>) p1 p2 = Parser (\s ->
        case runParser p1 s of
            Nothing -> runParser p2 s
            res -> res
        )

instance Monad Parser where
    return = pure
    -- | Calls "parser2" using what "parser1" parsed
    (>>=) parser1 parser2 = Parser (\s -> do
        (x,rest) <- runParser parser1 s
        runParser (parser2 x) rest
        )

-- | Calls "parser1" and calls "parser2" on what "parser1" didn't parse
-- and returns a tupple of "parser1" and "parser2"
(<&>) :: Parser a -> Parser b -> Parser (a, b)
(<&>) p1 p2 = Parser (\s -> do
            (c1, rest) <- runParser p1 s
            (c2, finalRest) <- runParser p2 rest
            Just((c1, c2), finalRest)
    )
-- | XOR parser
(<^>) p1 p2 = Parser $ \s -> case runParser p1 s of
            Nothing -> runParser p2 s
            res -> case runParser p2 s of
                Nothing -> res 
                _ -> Nothing

-- | Calls "p1", and passing "p2" the parsed value.
-- Returns the result of "p2"
(<%>) :: Parser String -> Parser a -> Parser a
p1 <%> p2 = Parser (\s -> do
    (parsed1, rest1) <- runParser p1 s
    (parsed2, rest2) <- runParser p2 parsed1
    Just (parsed2, rest2++rest1)
    )

-- | Parse the first element of the stream
parseHead :: Parser Char
parseHead = parseIf (const True)

-- | If the argument is the first element in the string, returns it
-- Otherwise, returns 'Nothing'
parseChar :: Char -> Parser Char
parseChar expected = Parser charParser
    where
        charParser :: ParserFunction Char
        charParser s = if not (null s) && head s == expected
            then Just (expected, tail s)
            else Nothing

-- | If one char of the first argument is the first element in the string, returns it
-- Otherwise, returns 'Nothing'
parseAnyChar :: String -> Parser Char
parseAnyChar needles = parseIf (`elem` needles)


-- | Calls '<&>' and applies "func" on the result
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func p1 p2 = Parser $ \s -> do
        ((a, b), rest) <- runParser (p1 <&> p2) s
        Just (func a b, rest)

-- | While "p" doesn't returns 'Nothing', it is called on the rest
-- Returns an array of parsed values. Never returns 'Nothing'
parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \s ->
    case runParser p s of
        Nothing -> Just ([], s)
        Just (p1, rest) -> runParser ((p1 :) <$> parseMany p) rest

-- | While "p" doesn't returns 'Nothing', it is called on the rest
-- Returns an array of parsed values. Returns 'Nothing' if the array is empty
parseSome :: Parser a -> Parser [a]
parseSome p1 = uncurry (:) <$> (p1 <&> parseMany p1)

-- | Parse unsigned integer from string
parseUInt :: Parser Integer
parseUInt = read <$> parseSome (parseAnyChar ['0'..'9'])

-- | Parse signed integer from string
parseInt :: Parser Integer
parseInt = Parser intParser
    where
        intParser :: ParserFunction Integer
        intParser s = do
            (signs, rest) <- runParser (parseMany (parseChar '-')) s
            runParser ((\nb-> nb * ((-1) ^ length signs)) <$> parseUInt) rest

-- | Parse floating point number from string, returns 'Nothing' if is NOT a floating point number
parseFloat :: Parser Float
parseFloat = Parser $ \s -> do
    (parsedInt, rest) <- runParser parseInt s
    (parsedDecimal, rest2) <- runParser parseDecimal rest
    res <- readMaybe (show parsedInt ++ parsedDecimal) :: Maybe Float
    return (res, rest2)
    where
        parseDecimal = uncurry (:) <$> (parseChar '.' <&> parseSome (parseAnyChar ['0'..'9']))
    

-- | Extract what's in the parenthesis
parseScope :: (Char, Char) -> Parser String
parseScope (begin, end) = Parser $ \s -> do
    (_, str) <- runParser parseWhitespaces s
    (parsed, rest) <- extractParenthesis str 0
    if not (null parsed) && head parsed == begin then Just (tail(init parsed), rest)
    else Nothing
    where
        extractParenthesis :: String -> Int -> Maybe (String, String)
        extractParenthesis [] 0 = Just ([], [])
        extractParenthesis [] _ = Nothing
        extractParenthesis (c:rest) 0 | c == end = Nothing
                                      | c == begin = do
                (parsed, newRest) <- extractParenthesis rest 1
                return (begin:parsed, newRest)
                                      | otherwise = Just ("", c:rest)
        extractParenthesis (c:rest) depth = if depth < 0 then Nothing else do
                (parsed, newRest) <- extractParenthesis rest (depth + getDepthOffset c)
                return (c:parsed, newRest)
        getDepthOffset :: Char -> Int
        getDepthOffset c | c == begin = 1
                         | c == end = -1
                         | otherwise = 0



-- | Parse anything that is a whitespace (using 'isSpace')
parseWhitespaces :: Parser String
parseWhitespaces = parseWhile isSpace <|> pure ""

-- | Parse String While function returns true, Nothing if 'Nothing' was parsed
parseWhile :: (Char -> Bool) -> Parser String
parseWhile f = parseSome $ parseIf f

-- | Parse String if function returns true
parseIf :: (Char -> Bool) -> Parser Char
parseIf f = Parser $ \s -> case s of
    "" -> Nothing
    _ -> if f first then Just (first, tail s) else Nothing
        where
            first = head s

-- | Run parser n times
parseN :: Int -> Parser a -> Parser [a]
parseN n p = Parser $ \s -> if n <= 0 then Just ([], s)
    else do
        (parsed, res) <- runParser p s
        runParser ((parsed: ) <$> parseN (n - 1) p) res

-- | Parse the string from the stream if it exists
parseString :: String -> Parser String
parseString string = Parser $ \s -> case string of
    "" -> Just ("", s)
    (char:rest) -> runParser (uncurry (:) <$> (parseChar char <&> parseString rest)) s

-- | Parse anything that is not a whitespace
parseWord :: Parser String
parseWord = parseWhile isNotSpace
    where
        isNotSpace :: Char -> Bool
        isNotSpace c = not $ isSpace c
