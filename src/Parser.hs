module Parser where

import Data.Maybe (isNothing)
import Text.Read
import Data.Char
import Control.Applicative ( Alternative((<|>), empty), some )

-- | A 'ParserFunction' function take a string a parameter,
-- maybe returns a successfull value of type 'a' tuppled with a string (the rest of the string)
type ParserFunction a = String -> Maybe (a, String)

-- | A 'Parser' of type 'a' holds a 'ParserFunction' of same type.
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
    -- | Calls 'parser2' using what 'parser1' parsed
    (>>=) parser1 parser2 = Parser (\s -> do
        (x,rest) <- runParser parser1 s
        runParser (parser2 x) rest
        )

-- | Calls 'parser1' and calls 'parser2' on what 'parser1' didn't parse
-- and returns a tupple of 'parser1' and 'parser2'
(<&>) :: Parser a -> Parser b -> Parser (a, b)
(<&>) p1 p2 = Parser (\s -> do
            (c1, rest) <- runParser p1 s
            (c2, finalRest) <- runParser p2 rest
            Just((c1, c2), finalRest)
    )

(<%>) :: Parser String -> Parser a -> Parser a
-- | Calls 'p1', and passing 'p2' the parsed value.
-- Returns the result of 'p2'
p1 <%> p2 = Parser (\s -> do
    (parsed1, rest1) <- runParser p1 s
    (parsed2, rest2) <- runParser p2 parsed1
    Just (parsed2, rest2++rest1)
    )

-- | If the argument is the first element in the string, returns it
-- Otherwise, returns nothing
parseChar :: Char -> Parser Char
parseChar expected = Parser charParser
    where
        charParser :: ParserFunction Char
        charParser s
            | null s = Nothing
            | head s == expected = Just (expected, tail s)
            | otherwise = Nothing

-- | If one char of the first argument is the first element in the string, returns it
-- Otherwise, returns nothing
parseAnyChar :: String -> Parser Char
parseAnyChar needles = Parser anyCharParser
    where
        anyCharParser :: ParserFunction Char 
        anyCharParser s
            | null s = Nothing
            | head s `elem` needles = Just (head s, tail s)
            | otherwise = Nothing 


-- | Calls '<&>' and applies 'func' on the result
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func p1 p2 = Parser (\s -> do
        ((a, b), rest) <- runParser (p1 <&> p2) s
        Just (func a b, rest)
    )

-- | While 'p' doesn't returns 'Nothing', it is called on the rest
-- Returns an array of parsed values. Never returns 'Nothing'
parseMany :: Parser a -> Parser [a]
parseMany p = Parser {runParser = \s -> 
    case runParser p s of
        Nothing -> Just ([], s)
        Just (p1, rest) -> runParser ((\p2 -> p1 : p2) <$> parseMany p) rest
    }

-- | While 'p' doesn't returns 'Nothing', it is called on the rest
-- Returns an array of parsed values. Returns 'Nothing' if the array is empty
parseSome :: Parser a -> Parser [a]
parseSome p1 = (\(a,b) -> a:b) <$> (p1 <&> parseMany p1)

-- | Parse unsigned integer from string
parseUInt :: Parser Integer
parseUInt = Parser $ \string -> 
    runParser (read <$> (parseSome (parseAnyChar ['0'..'9']))) string

-- | Parse signed integer from string
parseInt :: Parser Integer
parseInt = Parser intParser
    where
        intParser :: ParserFunction Integer
        intParser s = do
            (signs, rest) <- runParser (parseMany (parseChar '-')) s
            runParser ((\nb-> nb * ((-1) ^ length signs)) <$> parseUInt) rest

-- | Parse whitespacesfrom string
parseWhiteSpaces :: Parser String
parseWhiteSpaces = parseMany (parseAnyChar " \t")

-- Extracting parenthesis

_getDepthOffset :: Char -> Int 
_getDepthOffset '(' = 1
_getDepthOffset ')' = -1
_getDepthOffset _ = 0

-- If the string doesnt start by parenthesis, returns nothing
_extractParenthesis ::  String -> Int -> Maybe (String, String)
_extractParenthesis [] 0 = Just ([], [])
_extractParenthesis [] _ = Nothing 
_extractParenthesis (')':rest) 0 = Nothing
_extractParenthesis ('(':rest) 0 =  do
        (parsed, newRest) <- _extractParenthesis rest 1
        return ('(':parsed, newRest)
_extractParenthesis (c:rest) 0 = Just ("", c:rest) 
_extractParenthesis (c:rest) depth = if depth < 0 then Nothing else do
        (parsed, newRest) <- _extractParenthesis rest (depth + _getDepthOffset c) 
        return (c:parsed, newRest)

parseParenthesis :: Parser String
parseParenthesis = Parser $ \s -> do
    (_, str) <- runParser parseWhiteSpaces s
    res <- _extractParenthesis str 0
    case res of
        ('(':parsed, rest) -> Just (init parsed, rest)
        _ -> Nothing


-- | Parse anything that is a whitespace
parseWhitespaces :: Parser String
parseWhitespaces = _parseWhile isSpace
        
_parseWhile :: (Char -> Bool) -> Parser String
_parseWhile f = Parser $ \s -> case s of
    "" -> Just ("", "")
    (a:b) -> if f a then runParser ((a:) <$> _parseWhile f) b
             else Just ("", a:b)
--parseWord :: Parser String
--parseWord = Parser $ \s -> case runParser parseWhitespaces s of
--    Just ("", "") -> Just ([], "")
--    Just ("", a:b) -> runParser ((a :) <$> parseWord) b
--    Just (a, rest) -> Just ([], a ++ rest)
--    _ -> Nothing

-- | Parse anything that is not a whitespace
parseWord :: Parser String
parseWord = some _parseNotSpace

_parseNotSpace :: Parser Char
_parseNotSpace = Parser anyCharParser
    where
        anyCharParser :: ParserFunction Char 
        anyCharParser s
            | null s = Nothing
            | isSpace (head s) = Nothing
            | otherwise = Just (head s, tail s)