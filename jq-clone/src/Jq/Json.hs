module Jq.Json where

import Debug.Trace

data JSON =
    JNull |
    JString { str :: String } |
    JInteger { int :: Int} |
    JDouble { dbl :: Double} |
    JBool { bool :: Bool } |
    JObject { pairs :: [(String, JSON)] } |
    JArray { listOfValues :: [JSON] } |
    JNewObjectArray { listOfValues :: [JSON]} |
    JEmpty
    -- deriving (Show)

instance Show JSON where
  show JEmpty = ""
  show JNull = "null"
  show (JString strng) = "\"" ++ strng ++ "\""
  show (JInteger num) = show num
  show (JDouble x) = show x
  show (JBool b) = if b then "true" else "false"
  show (JArray []) = "[]"
  show (JObject []) = "{}"
  show something = showSomethingElse something 0 False

showSomethingElse :: JSON -> Int -> Bool -> String
-- Empty object
showSomethingElse (JObject []) n bool = if bool
                                          then "{}"
                                          else concat (replicate n "  ") ++ "{}"
-- One key value pair
showSomethingElse (JObject [(key,value)]) n bool = someString
  where
    someString = case value of 
      JArray _ -> concat (replicate n "  ") ++ "{\n" ++ concat (replicate (n+1) "  ") ++ "\"" ++ key ++ "\": " ++ showSomethingElse value (n+1) True ++ "\n" ++ concat (replicate n "  ") ++ "}"
      JObject _ -> if bool 
                    then "{\n" ++ concat (replicate (n+1) "  ") ++ "\"" ++ key ++ "\": " ++ showSomethingElse value (n+1) True ++ "\n" ++ concat (replicate n "  ") ++ "}"
                    else concat (replicate n "  ") ++ "{\n" ++ concat (replicate (n+1) "  ") ++ "\"" ++ key ++ "\": " ++ showSomethingElse value (n+1) True ++ "\n" ++ concat (replicate n "  ") ++ "}"
      _ -> if bool
              then "{\n" ++ concat (replicate (n+1) "  ") ++ "\"" ++ key ++ "\": " ++ showSomethingElse value (n+1) True ++ "\n" ++ concat (replicate n "  ") ++ "}"
              else concat (replicate n "  ") ++ "{\n" ++ concat (replicate (n+1) "  ") ++ "\"" ++ key ++ "\": " ++ showSomethingElse value (n+1) True ++ "\n" ++ concat (replicate n "  ") ++ "}"
-- Many key value pairs
showSomethingElse (JObject ((key,value):xs)) n bool = someString
  where
    someString = case value of 
      JArray _ -> concat (replicate n "  ") ++ "{\n" ++ concat (replicate (n+1) "  ") ++ "\"" ++ key ++ "\": " ++ showSomethingElse value (n+1) True ++ ",\n" ++ showEveryObject xs (n+1) ++ "\n" ++ concat (replicate n "  ") ++ "}"
      JObject _ -> if bool
                      then "{\n" ++ concat (replicate (n+1) "  ") ++ "\"" ++ key ++ "\": " ++ showSomethingElse value (n+1) True ++ ",\n" ++ showEveryObject xs (n+1) ++ "\n" ++ concat (replicate n "  ") ++ "}"
                      else concat (replicate n "  ") ++ "{\n" ++ concat (replicate (n+1) "  ") ++ "\"" ++ key ++ "\": " ++ showSomethingElse value (n+1) True ++ ",\n" ++ showEveryObject xs (n+1) ++ "\n" ++ concat (replicate n "  ") ++ "}"
      _ -> concat (replicate n "  ") ++ "{\n" ++ concat (replicate (n+1) "  ") ++ "\"" ++ key ++ "\": " ++ show value ++ ",\n" ++ showEveryObject xs (n+1) ++ "\n" ++ concat (replicate n "  ") ++ "}"
-- Empty array
showSomethingElse (JArray []) n _ = concat (replicate n "  ") ++ "[]"
-- One array value
showSomethingElse (JArray [x]) n bool = someString
  where
    someString = case x of 
      JArray _ -> if bool 
                    then "[\n" ++ showSomethingElse x (n+1) False ++ "\n" ++ concat (replicate n "  ") ++ "]"
                    else concat (replicate n "  ") ++ "[\n" ++ showSomethingElse x (n+1) False ++ "\n" ++ concat (replicate n "  ") ++ "]"
      JObject _ -> concat (replicate n "  ") ++ "[\n" ++ showSomethingElse x (n+1) False ++ "\n" ++ concat (replicate n "  ") ++ "]"
      somethingElse -> if bool 
                    then "[\n" ++ showSomethingElse somethingElse (n+1) False ++ "\n" ++ concat (replicate n "  ") ++ "]"
                    else concat (replicate n "  ") ++ "[\n" ++ showSomethingElse somethingElse (n+1) False ++ "\n" ++ concat (replicate n "  ") ++ "]"
-- Many array values
showSomethingElse (JArray (x:xs)) n bool = someString
  where
    someString = case x of 
      JArray _ -> if bool 
                    then "[\n" ++ showEverySomethingElse [x] n ++ ",\n" ++ showEverySomethingElse xs n ++ "\n" ++ concat (replicate n "  ") ++ "]"
                    else concat (replicate n "  ") ++ "[\n" ++ showEverySomethingElse [x] n ++ ",\n" ++ showEverySomethingElse xs n ++ "\n" ++ concat (replicate n "  ") ++ "]"
      JObject _ -> concat (replicate n "  ") ++ "[\n" ++ showEverySomethingElse [x] n ++ ",\n" ++ showEverySomethingElse xs n ++ "\n" ++ concat (replicate n "  ") ++ "]"
      somethingElse -> if bool 
                    then "[\n" ++ showEverySomethingElse [somethingElse] n  ++ ",\n" ++ showEverySomethingElse xs n ++ "\n" ++ concat (replicate n "  ") ++ "]"
                    else concat (replicate n "  ") ++ "[\n" ++ showEverySomethingElse [somethingElse] n  ++ ",\n" ++ showEverySomethingElse xs n ++ "\n" ++ concat (replicate n "  ") ++ "]"

-- Creating List 
showSomethingElse (JNewObjectArray [x]) n bool = showSomethingElse x n bool
showSomethingElse (JNewObjectArray list) n bool = if bool
                                                      then "[\n" ++ createList list n ++ "\n" ++ concat (replicate n "  ") ++ "]"
                                                      else concat (replicate n "  ") ++ "[\n" ++ createList list n ++ "\n" ++ concat (replicate n "  ") ++ "]"
-- Something el se
showSomethingElse something n bool = if bool
                                        then show something 
                                        else concat (replicate n "  ") ++ show something

showEverySomethingElse :: [JSON] -> Int -> String
showEverySomethingElse [] n = concat (replicate n "  ") ++ "[]"
showEverySomethingElse [x] n =  showSomethingElse x (n+1) False
showEverySomethingElse (x:xs) n = showSomethingElse x (n+1) False ++ ",\n" ++ showEverySomethingElse xs n

showEveryObject :: [(String, JSON)] -> Int -> String
showEveryObject [] _ = ""
showEveryObject [(key, value)] n = concat (replicate n "  ") ++ "\"" ++ key ++ "\": " ++ showSomethingElse value n True
showEveryObject ((key, value):xs) n = concat (replicate n "  ") ++ "\"" ++ key ++ "\": " ++ showSomethingElse value n True ++ ",\n" ++ showEveryObject xs n 

createList :: [JSON] -> Int -> String
createList [] _ = ""
createList [x] n = showSomethingElse x (n+1) False 
createList (x:xs) n = showSomethingElse x (n+1) False ++ ",\n" ++ createList xs n