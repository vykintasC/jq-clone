module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Debug.Trace
import Jq.JParser

parseFilter :: Parser Filter 
parseFilter = 
  do
    do
      _ <- symbol "{}"
      return (ObjectBuild [])
    <|>
    do
      _ <- symbol "{"
      -- get new object keys 
      firstPair <- parseCreateObject
      theRest <- many (do
        _ <- symbol "," 
        parseCreateObject)
      _ <- symbol "}"
      return (ObjectBuild (firstPair : theRest))
    <|>
     do
      _ <- symbol "{"
      -- get words for the new object to be created
      firstWord <- parseWord
      theRest <- many (do
        _ <- symbol "," 
        parseWord)
      _ <- symbol "}"
      return (ObjectBuildFromWords (firstWord : theRest))
    <|>
    do
      parsePipe  <|> parseComma <|> parseAtomicFilter

parsePipe :: Parser Filter
parsePipe = 
  do
    firstFilter <- parseComma <|> parseAtomicFilter <|> parseFilter
    do
      do
        theRest <- many (do 
          _ <- symbol "|"
          parseComma <|> parseAtomicFilter)
        return (Pipe (firstFilter : theRest))
      <|>
      return firstFilter
    
  
parseComma :: Parser Filter
parseComma = 
  do
    firstFilter <- parseAtomicFilter <|> parseFilter
    do
      do
        theRest <- some (do 
          _ <- symbol ","
          parseAtomicFilter <|> parseFilter)
        return (Comma (firstFilter : theRest))
      <|>
      return firstFilter
      
parseAtomicFilter :: Parser Filter
parseAtomicFilter = 
  do
    _ <- symbol "("
    inside <- parsePipe <|> parseAtomicFilter
    _ <- symbol ")"
    return (Parentheses inside)
  <|>
  do
    _ <- symbol ".[]?"
    return IteratorOpt
  <|>
  do
    _ <- symbol ".[]"
    return IteratorAll
  <|>
  do
    _ <- symbol ".["
    firstIndex <- integer
    theRest <- many (do 
      _ <- symbol ","
      integer)
    _ <- symbol "]"
    return (Iterator (firstIndex : theRest))
  <|>
  do
    _ <- symbol ".["
    sliceFrom <- integer 
    _ <- symbol ":"
    sliceTo <- integer 
    _ <- symbol "]"
    return (Slice (sliceFrom, sliceTo))
  <|>
  do 
    parseIndexWithIteration
  <|>
  do 
    _ <- symbol "."
    return Identity

parseIndexWithIteration :: Parser Filter
parseIndexWithIteration = 
  do
    firstIndex <- parseIndexGenOpt <|> parseIndexOpt <|> parseIndexGen <|> parseIndex 
    do
      do 
        _ <- symbol "[]"
        theRest <- many (do parseAtomicFilter)
        return (Pipe ([IndexObj [firstIndex], IteratorAll] ++ theRest))
      <|>
      do
        _ <- symbol "["
        firstInteger <- integer 
        theRest <- many (do 
          _ <- symbol ","
          integer)
        _ <- symbol "]"
        return (Pipe [IndexObj [firstIndex], Iterator (firstInteger : theRest)])
      <|>
      do
        filter <- parseIndexWithIteration
        return (Pipe [IndexObj [firstIndex], filter])
      <|>
      do
        theRest <- many (do parseIndexGenOpt <|> parseIndexOpt <|> parseIndexGen <|> parseIndex)
        return (IndexObj (firstIndex : theRest))

        
parseIndexOpt :: Parser String
parseIndexOpt =
  do
    _ <- char '.'
    index <- some (do alphanum <|> char '_')
    _ <- char '?'
    return (index ++ "?")

parseIndexGenOpt :: Parser String
parseIndexGenOpt =
  do
    _ <- char '.' 
    _ <- symbol "[\""
    index <- some (do letter)
    _ <- symbol "\"]?"
    return (index ++ "?")

parseIndexGen :: Parser String
parseIndexGen =
  do
    _ <- char '.' 
    _ <- symbol "[\""
    index <- some (do letter)
    _ <- symbol "\"]"
    return (index)

parseIndex :: Parser String
parseIndex =
  do
    _ <- char '.'
    index <- some (do alphanum <|> char '_')
    return (index)

parseCreateObject :: Parser (String, Filter)
parseCreateObject = 
  do 
    -- parse the key
    key <- parseString
    -- parse the filter
    _ <- symbol ":"
    value <- 
      do
        _ <- symbol "["
        first <- parseJSON
        list <- many (do 
          _ <- string "," 
          parseJSON)
        _ <- symbol "]"
        let finalList = if checkListEntries (first : list) then (first : list) else error "List entries are not the same."
        return (SimpleValue (show finalList))
      <|>
      do 
        _ <- symbol "["
        filt <- parseFilter
        _ <- symbol "]"
        return filt
      <|>
      do
        parseSimpleValue 
    return (key, value)

parseString :: Parser String 
parseString =
  do
    _ <- symbol "\""
    firstLetter <- alphanum <|> getSpecialChars
    theRest <- many (do alphanum <|> getSpecialChars)
    _ <- symbol "\""
    return (firstLetter : theRest)

parseWord :: Parser String 
parseWord =
  do
    firstLetter <- alphanum
    theRest <- many (do alphanum)
    return (firstLetter : theRest)
    
parseSimpleValue :: Parser Filter
parseSimpleValue = 
  do
    justString <- parseString
    return (SimpleValue justString)
  <|>
  do 
    _ <- string "true" 
    return (SimpleValue "true")
  <|>
  do
    _ <- string "false"
    return (SimpleValue "false")
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
    return (SimpleValue (show resultingNumber))
  <|>
  do
    beforeComma <- integer
    _ <- symbol "."
    afterComma <- integer
    let resultingNumber = show beforeComma ++ "." ++ show afterComma
    return (SimpleValue resultingNumber)
  <|>
  do 
    num <- integer
    return (SimpleValue (show num))
  <|>
  do 
    parseFilter


parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ -> 
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e