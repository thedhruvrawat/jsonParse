{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Data.Char
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

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined 

