module Main where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec as P (
    Parser,
    char,
    digit,
    many,
    many1,
    noneOf,
    oneOf,
    parse,
    sepBy,
    spaces,
    string,
    try,
    (<|>),
 )

data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonNumber Integer
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq, Ord)

parseNull :: P.Parser JsonValue
parseNull = P.string "null" >> return JsonNull

parseBool :: P.Parser JsonValue
parseBool = do
    b <-
        P.try (P.string "true" >> return True)
            <|> (P.string "false" >> return False)
    return $ JsonBool b

parseInteger :: P.Parser JsonValue
parseInteger = do
    i <- P.many1 P.digit
    return $ JsonNumber (read i)

parseEscapeCharacter :: P.Parser Char
parseEscapeCharacter = do
    P.char '\\'
    c <- P.oneOf "\"\\/bfnrt"
    return $ case c of
        '"' -> '"'
        '\\' -> '\\'
        '/' -> '/'
        'b' -> '\b'
        'f' -> '\f'
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'

parseString :: P.Parser JsonValue
parseString = do
    P.char '"'
    s <- P.many (parseEscapeCharacter <|> P.noneOf "\"")
    P.char '"'
    return $ JsonString s

parseArray :: P.Parser JsonValue
parseArray = do
    P.char '['
    elems <- P.sepBy parseJson (P.char ',')
    P.char ']'
    return $ JsonArray elems

parseObject :: P.Parser JsonValue
parseObject = do
    P.char '{'
    elems <- P.sepBy parseField (P.char ',')
    P.char '}'
    return $ JsonObject elems

parseField :: P.Parser (String, JsonValue)
parseField = do
    P.char '"'
    key <- P.many (P.noneOf "\"")
    P.char '"'
    P.char ':'
    value <- parseJson
    return (key, value)

parseJson :: P.Parser JsonValue
parseJson =
    P.spaces
        *> parseNull
            <|> parseBool
            <|> parseInteger
            <|> parseString
            <|> parseArray
            <|> parseObject
        <* P.spaces

parseFile :: String -> IO ()
parseFile filename = do
    input <- readFile filename
    case P.parse parseJson "" input of
        Right json -> print json
        Left err -> print err

parseInput :: IO ()
parseInput = do
    putStrLn "Enter JSON to parse:"
    input <- getLine
    case P.parse parseJson "" input of
        Right json -> print json
        Left err -> print err

main :: IO ()
main = do
    args <- getArgs
    if null args then parseInput else parseFile (head args)
