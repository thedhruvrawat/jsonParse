{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.Char
import Control.Applicative

--defining Abstract Syntax Tree
data JsonValue 
    = JsonNull
    | JsonBool Bool 
    | JsonNumber Integer --no support for float
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a) --makes parsers composable into chains
    --Maybe allows parser to work with unparsable values; but no proper error report
    }

--Proving parser is a functor, so that it becomes applicative
instance Functor Parser where 
    fmap f (Parser p) = 
        Parser $ \input -> do
            (input', x) <- p input
            Just (input', f x)

--Proving parser is applicative
instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = 
        Parser $ \input -> do
         (input', f) <- p1 input 
         (input'', a) <- p2 input'
         Just (input'', f a)

--Proving parser is alternative
instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = 
        Parser $ \input -> p1 input <|> p2 input 
        --take input into first parser, if it fails, put same input into second parser and try to parse that

--Parses single char
charP :: Char -> Parser Char 
charP x = Parser f
    where 
      f (y:ys)  
        | y == x = Just (ys, x)
        | otherwise = Nothing
      f [] = Nothing

--Parses a sequence of characters
stringP :: String -> Parser String --gave a list of parsers, we want a parser of lists, need to turn it inside out
stringP = sequenceA . map charP --will only work if we prove to compiler that parser is applicative

--Parsing null
jsonNull :: Parser JsonValue
jsonNull = (\_ -> JsonNull) <$> stringP "null"

--Parsing bool
jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
    where f "true"  = JsonBool True
          f "false" = JsonBool False
          --this should never happen
          f _       = undefined

--Parsing numbers
spanP :: (Char -> Bool) -> Parser String
spanP f = 
    Parser $ \input -> 
        let (token, rest) = span f input 
            in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = 
    Parser $ \input -> do
        (input', xs) <- p input
        if null xs
            then Nothing
            else Just (input', xs)


jsonNumber :: Parser JsonValue
jsonNumber = f <$> spanP isDigit 
    where f ds = JsonNumber $ read ds

--Parsing string
stringLiteral :: Parser String
stringLiteral = spanP (/= '"') --doesn't support escape 

jsonString :: Parser JsonValue
jsonString = JsonString <$> (charP '"' *> stringLiteral <* charP '"')

--Parsing arrays
ws :: Parser String --whitespace
ws = spanP isSpace 

sepBy :: Parser a -> Parser b -> Parser [b] --separator
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
    where 
        elements = sepBy (ws *> charP ',' <* ws) jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray

main :: IO ()
main = undefined 

