{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# LANGUAGE LambdaCase #-}
module Main where

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

--Parses single char
charP :: Char -> Parser Char 
charP x = Parser f
    where 
      f input = 
        case input of 
            y:ys 
              | y == x -> Just (ys, x)
            _ -> Nothing

--Parsing null
jsonNull :: Parser JsonValue
jsonNull = undefined 

jsonValue :: Parser JsonValue
jsonValue = undefined

main :: IO ()
main = undefined 

