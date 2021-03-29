module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser
import Jq.Json
import Debug.Trace

parseFilter :: Parser Filter 
parseFilter = 
  do
    do
      -- Check if we are creating an object and it is empty
      _ <- symbol "{}"
      return (ObjectBuild [])
    <|>
    do
      -- Check if we are creating an object and it is empty
      _ <- symbol "[]"
      return (ObjectListCreation [])
    <|>
    do
      -- Check if we are creating an object and need to look for keys
      _ <- symbol "{" 
      firstPair <- parseCreateObject
      theRest <- many (do
        _ <- symbol "," 
        parseCreateObject)
      _ <- symbol "}"
      return (ObjectBuild (firstPair : theRest))
    <|>
    do 
      -- Check for list with filters
      _ <- symbol "["
      listFilter <- parsePipe
      _ <- symbol "]"
      return (ObjectList listFilter)
    <|>
    do 
      -- Check for list with some values
      _ <- symbol "["
      firstThing <- parseFilter
      theRest <- many (do
        _ <- symbol "," 
        parseFilter)
      _ <- symbol "]"
      return (ObjectListCreation (firstThing : theRest))
    <|>
    do
      parsePipe  <|> parseComma <|> parseAtomicFilter

-- Parse pipe |
parsePipe :: Parser Filter
parsePipe = 
  do
    -- Find first filter
    firstFilter <- parseComma <|> parseAtomicFilter <|> parseFilter
    do
      do
        -- Check if there are pipes connecting them
        theRest <- many (do 
          _ <- symbol "|"
          parseComma <|> parseAtomicFilter)
        return (Pipe (firstFilter : theRest))
      <|>
      -- Else return the first found
      return (Pipe [firstFilter])
    
  
-- Parse comma, such as .a,.b,.c   
parseComma :: Parser Filter
parseComma = 
  do
    -- Parse first filter
    firstFilter <- parseAtomicFilter <|> parseFilter
    do
      do
        -- Find the rest if they exist
        theRest <- some (do 
          _ <- symbol ","
          parseAtomicFilter <|> parseFilter)
        return (Comma (firstFilter : theRest))
      <|>
      -- Else just return the first filter
      return firstFilter
      
-- Parse not compound filters
parseAtomicFilter :: Parser Filter
parseAtomicFilter = 
  do
    -- Parse parentheses. There can be anything in the parentheses.
    -- Could be a simple filter, or another parentheses or a pipe, does not matter.
    _ <- symbol "("
    insideParentheses <- parsePipe <|> parseAtomicFilter
    _ <- symbol ")"
    return (Parentheses insideParentheses)
  <|>
  do
    -- Optional iteration
    _ <- symbol ".[]?"
    return (IteratorOpt [])
  <|>
  do
    -- Not optional iteration
    _ <- symbol ".[]"
    return IteratorAll
  <|>
  do
    -- Indices of a list
    _ <- symbol ".["
    firstIndex <- integer
    theRest <- many (do 
      _ <- symbol ","
      integer)
    _ <- symbol "]?"
    return (IteratorOpt (firstIndex : theRest))
  <|>
  do
    -- Indices of a list
    _ <- symbol ".["
    firstIndex <- integer
    theRest <- many (do 
      _ <- symbol ","
      integer)
    _ <- symbol "]"
    return (Iterator (firstIndex : theRest))
  <|>
  do
    parseSliceOpt
  <|>  
  do
    parseSlice
  <|>
  do 
    -- Parse sugar such as .foo[1,2] which just means .foo | .[1,2]
    parseIndexWithIteration
  <|>
  do 
    -- Finally identity
    _ <- symbol "."
    return Identity
  <|>
  do 
    parseSimpleValue
  <|>
    error "Your provided filter is bad."


-- Parse sugar such as .foo[1,2] which just means .foo | .[1,2] or just a simple .foo.bar sequence
parseIndexWithIteration :: Parser Filter
parseIndexWithIteration = 
  do
    -- Get the first part, i.e. .foo
    firstIndex <- parseIndexGenOpt <|> parseIndexOpt <|> parseIndexGen <|> parseIndex <|> parseIndexSemiGen <|> parseIndexSemiGenOpt
    do
       do 
        -- There is []? after, meaning full iteration
        _ <- symbol "[]?"
        -- Parse the rest, could be a compound such as  .foo[].bar[]?
        theRest <- many (do parseAtomicFilter)
        return (Pipe ([IndexObj [firstIndex], IteratorOpt []] ++ theRest))
      <|>
      do 
        -- There is [] after, meaning full iteration
        _ <- symbol "[]"
        -- Parse the rest, could be a compound such as  .foo[].bar[]
        theRest <- many (do parseAtomicFilter)
        return (Pipe ([IndexObj [firstIndex], IteratorAll] ++ theRest))
      <|>
      do
        -- There are indices [1,2]? in iteration
        _ <- symbol "["
        firstInteger <- integer 
        -- The rest also could have indices, i.e. .foo[1,2].bar[1,2]?
        theRest <- many (do 
          _ <- symbol ","
          integer)
        _ <- symbol "]?"
        return (Pipe [IndexObj [firstIndex], IteratorOpt (firstInteger : theRest)])
      <|>
      do
        -- There are indices [1,2] in iteration
        _ <- symbol "["
        firstInteger <- integer 
        -- The rest also could have indices, i.e. .foo[1,2].bar[1,2]
        theRest <- many (do 
          _ <- symbol ","
          integer)
        _ <- symbol "]"
        return (Pipe [IndexObj [firstIndex], Iterator (firstInteger : theRest)])
      <|>
      do
        -- Slicing Optional 
        slicer <- parseSliceOpt
        return (Pipe [IndexObj [firstIndex], slicer])
      <|>
      do
        -- Slicing normal
        slicer <- parseSlice
        return (Pipe [IndexObj [firstIndex], slicer])
      <|>
      do
        -- It can be a combination of full iterations and indexed iterations, this also allows for .foo.bar[].[] which means it will take an array existing somewhere in bar and iterate over it.
        filt <- parseIndexWithIteration
        return (Pipe [IndexObj [firstIndex], filt])
      <|>
      do
        -- Parse the sequence .foo.bar without iterations
        theRest <- many (do parseIndexGenOpt <|> parseIndexOpt <|> parseIndexGen <|> parseIndex <|> parseIndexSemiGen <|> parseIndexSemiGenOpt)
        return (IndexObj (firstIndex : theRest))

-- Parse optional index such as .foo?       
parseIndexOpt :: Parser (String, Bool)
parseIndexOpt =
  do
    _ <- char '.'
    index <- some (do alphanum <|> char '_')
    _ <- char '?'
    return (index, True)

-- Parse optional index such as .["foo"]?
parseIndexGenOpt :: Parser (String, Bool)
parseIndexGenOpt =
  do
    _ <- char '.' 
    _ <- symbol "[\""
    index <- some (do letter)
    _ <- symbol "\"]?"
    return (index, True)

-- Parse index such as ."foo"
parseIndexGen :: Parser (String, Bool)
parseIndexGen =
  do
    _ <- char '.' 
    _ <- symbol "[\""
    idx <- some (do letter)
    _ <- symbol "\"]"
    return (idx, False)
  
-- Parse index such as ."foo"?
parseIndexSemiGen :: Parser (String, Bool)
parseIndexSemiGen =
  do
    _ <- char '.' 
    _ <- symbol "\""
    idx <- some (do letter)
    _ <- symbol "\"?"
    return (idx, True)

parseIndexSemiGenOpt :: Parser (String, Bool)
parseIndexSemiGenOpt =
  do
    _ <- char '.' 
    _ <- symbol "\""
    idx <- some (do letter)
    _ <- symbol "\""
    return (idx, False)

-- Parse index such as .foo
parseIndex :: Parser (String, Bool)
parseIndex =
  do
    _ <- char '.'
    idx <- some (do alphanum <|> char '_')
    return (idx, False)

parseCreateObject :: Parser (Filter, Filter)
parseCreateObject = 
  do 
    -- Parse the key of the object
    key <- 
      do 
        parseString 
      <|>
      do
        parseFilter
    -- : means there is going to be something after
    _ <- symbol ":"
    -- Parse the value of the object
    value <- 
      do
        -- The value could be a list of some values
        _ <- symbol "["
        first <- parseJSON
        list <- many (do 
          _ <- string "," 
          parseJSON)
        _ <- symbol "]"
        return (SimpleValue (JArray (first : list)))
      <|>
      do 
        -- The value could be a filer, which needs extra parsing
        _ <- symbol "["
        filt <- parseFilter
        _ <- symbol "]"
        return filt
      <|>
      do
        -- The value could be a simple value such as 1, true, "word"
        parseSimpleValue
      <|>
      do
        parseAtomicFilter
    return (key, value)
  <|>
  do
    key <- parseString
    return (FWord key, FWord key)

--Slicing normal 
parseSlice :: Parser Filter 
parseSlice = 
  do
    -- Slicing the list 
    _ <- symbol ".["
    sliceFrom <- integer 
    _ <- symbol ":"
    sliceTo <- integer 
    _ <- symbol "]"
    return (Slice (show sliceFrom, show sliceTo))
  <|>
  do
    -- Slicing the list the first number is ommited
    _ <- symbol ".["
    _ <- symbol ":"
    sliceTo <- integer 
    _ <- symbol "]"
    return (Slice ("", show sliceTo))
  <|>
  do
    -- Slicing the list the second number is ommited
    _ <- symbol ".["
    sliceFrom <- integer 
    _ <- symbol ":"
    _ <- symbol "]"
    return (Slice (show sliceFrom, ""))

--Slicing optional
parseSliceOpt :: Parser Filter 
parseSliceOpt = 
  do
    slicer <- parseSlice
    case slicer of
      (Slice (sliceFrom, sliceTo)) -> do
                                      _ <- symbol "?"
                                      return (SliceOpt (sliceFrom, sliceTo))
      _ -> error (show slicer ++ "?  is not a valid filter.")

-- Function to parse strings such as "string!$#$^^1314"
parseString :: Parser Filter
parseString =
  do
    _ <- symbol "\""
    firstLetter <- alphanum <|> getSpecialChars
    theRest <- many (do alphanum <|> getSpecialChars)
    _ <- symbol "\""
    return (SimpleValue (JString (firstLetter : theRest)))
  -- Should we allow words without "" ??
  <|>
  do
    firstLetter <- alphanum
    theRest <- many (do alphanum)
    return (SimpleValue (JString (firstLetter : theRest)))
    
-- Parse simple value when creating an object
parseSimpleValue :: Parser Filter
parseSimpleValue = do SimpleValue <$> parseJSON


parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ -> 
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e