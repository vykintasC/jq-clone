module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Debug.Trace

parseJSON :: Parser JSON
parseJSON =
    do  
        -- Check for an empty object
        _ <- symbol "{}"
        return (JObject [])
    <|>
    do 
        -- Check for a non empty object
        _ <- symbol "{"
        first <- parseJObject
        theRest <- many (do
            _ <- string ","
            parseJObject)
        _ <- symbol "}"
        return (JObject (first : theRest))       
    <|>
    do 
        -- Check for an empty array
        _ <- symbol "[]"
        return (JArray [])
    <|>
    do
        -- Check for a non empty array
        _ <- symbol "["
        first <- parseJSON
        list <- many (do 
            _ <- string "," 
            parseJSON)
        _ <- symbol "]"
        return (JArray (first : list))
    <|>
    do
        -- Check for null
        _ <- symbol "null"
        return JNull 
    <|>
    do
        -- Check for true
        _ <- symbol "true"
        return (JBool True)
    <|>
    do
        -- Check for false
        _ <- symbol "false"
        return (JBool False)
    <|>
    do
        -- Check for a string
        _ <- symbol "\""
        firstLetter <- alphanum <|> getSpecialChars
        theRest <- many (do alphanum <|> getSpecialChars)
        _ <- symbol "\""
        let key = firstLetter : theRest
        return (JString key)
    <|>
    do
        -- Check for a double which is expressed in the E notation
        beforeComma <- integer -- Get integer before comma
        _ <- symbol "."
        afterComma <- integer -- Get integer after comma
        let floating = read (show beforeComma ++ "." ++ show afterComma) -- Translate it into a double
        _ <- string "E" -- If there is an E
        sign <- item -- Check for the sign, + or -
        power <- integer -- Check for the power
        let resultingNumber = if sign == '+' then floating * 10^power else floating * (1/(10^power)) -- Calculate the resulting double
        return (JDouble resultingNumber)
    <|>
    do
        -- Check for a double which is experssed in a normal way
        beforeComma <- integer
        _ <- symbol "."
        afterComma <- integer
        let resultingNumber = read (show beforeComma ++ "." ++ show afterComma)
        return (JDouble resultingNumber)
    <|>
    do
        -- Check for an integer
        JInteger <$> integer

-- In {} object, parse value for every key
parseJObject :: Parser (String, JSON)
parseJObject = do
    potentialKey <- parseJSON
    let key = case potentialKey of
                JString s -> s
                _ -> error "Key is not a string"
    _ <- symbol ":"
    value <- parseJSON
    return (key, value)

-- Get special char which could be inside a string
getSpecialChars :: Parser Char
getSpecialChars = 
    char ' ' <|>
    char '.' <|> 
    char '_' <|> 
    char ':' <|> 
    char '-' <|>
    char '\\' <|>
    char '+' <|>
    char '-' <|>
    char '=' <|>
    char '?' <|>
    char '>' <|>
    char '<' <|>
    char ',' <|>
    char ';' <|>
    char '[' <|>
    char ']' <|>
    char '}' <|>
    char '{' <|>
    char '|' <|>
    char '\'' <|>
    char '(' <|>
    char ')' <|>
    char '*' <|>
    char '&' <|>
    char '^' <|>
    char '%' <|>
    char '$' <|>
    char '#' <|>
    char '@' <|>
    char '!' <|>
    char '~' <|>
    char '/' <|>
    char '\'' <|>
    char '`'