module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Debug.Trace

parseJSON :: Parser JSON
parseJSON =
    do
        _ <- symbol "{}"
        return (JObject [])
    <|>
    do 
        _ <- symbol "{"
        first <- parseJObject
        theRest <- many (do
            _ <- string ","
            parseJObject)
        _ <- symbol "}"
        return (JObject (first : theRest))       
    <|>
    do 
        _ <- symbol "[]"
        return (JArray [])
    <|>
    do
        _ <- symbol "["
        first <- parseJSON
        list <- many (do 
            _ <- string "," 
            parseJSON)
        _ <- symbol "]"
        let finalList = if checkListEntries (first : list) then JArray (first : list) else error "List entries are not the same."
        return finalList
    <|>
    do
        _ <- symbol "null"
        return JNull 
    <|>
    do
        _ <- symbol "true"
        return (JBool True)
    <|>
    do
        _ <- symbol "false"
        return (JBool False)
    <|>
    do
        _ <- symbol "\""
        firstLetter <- alphanum <|> getSpecialChars
        theRest <- many (do alphanum <|> getSpecialChars)
        _ <- symbol "\""
        let key = firstLetter : theRest
        return (JString key)
    <|>
    do
        beforeComma <- integer
        _ <- symbol "."
        afterComma <- integer
        let floating = read (show beforeComma ++ "." ++ show afterComma)
        _ <- string "E"
        sign <- item
        power <- integer 
        let resultingNumber = if sign == '+' then floating * 10^power else floating * (1/(10^power))
        return (JDouble resultingNumber)
    <|>
    do
        beforeComma <- integer
        _ <- symbol "."
        afterComma <- integer
        let resultingNumber = read (show beforeComma ++ "." ++ show afterComma)
        return (JDouble resultingNumber)
    <|>
    do
        intg <- integer 
        return (JInteger intg)


checkListEntries :: [JSON] -> Bool
checkListEntries [] = True
checkListEntries (x:[]) = True 
checkListEntries (x:y:xs) = x == y && checkListEntries (y:xs)

parseJObject :: Parser (String, JSON)
parseJObject = do
    potentialKey <- parseJSON
    let key = case potentialKey of
                JString str -> str
                _ -> error "Key is not a string"
    _ <- symbol ":"
    value <- parseJSON
    return (key, value)

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
    char '/'