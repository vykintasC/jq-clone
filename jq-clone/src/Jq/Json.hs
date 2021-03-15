module Jq.Json where

data JSON =
    JNull |
    JString { str :: String } |
    JInteger { int :: Int} |
    JDouble { dbl :: Double} |
    JBool { bool :: Bool } |
    JObject { pairs :: [(String, JSON)] } |
    JArray { listOfValues :: [JSON] } |
    JNewObjectArray { listOfValues :: [JSON]}

instance Show JSON where
  show JNull = "null"
  show (JString str) = str
  show (JInteger num) = show num
  show (JDouble dbl) = show dbl
  show (JBool b) = if b then "true" else "false"
  show (JArray list) = "[" ++ renderJson (JArray list) ++ "]" 
  show (JNewObjectArray list) = renderJson (JArray list)
  show (JObject []) = "{}"
  show (JObject (x:xs)) = "{" ++ fst x ++ ":" ++ theRest ++ "}"
    where 
      theRest = if length xs == 0 then one else more
        where 
          one = show (snd x)
          list = createStringFromJObject xs
          num = length list - 1
          more = one ++ "," ++ take num list

renderJson :: JSON -> String
renderJson (JArray xs) = take num list
  where 
    list = createStringFromJArray xs
    num = length list - 1

createStringFromJArray :: [JSON] -> String
createStringFromJArray [] = ""
createStringFromJArray (x:[]) = show x ++ ","
createStringFromJArray (x:xs) = show x  ++ "," ++ createStringFromJArray xs

createStringFromJObject :: [(String, JSON)] -> String
createStringFromJObject [] = ""
createStringFromJObject (x:[]) = fst x ++ ":" ++ show (snd x) ++ ","
createStringFromJObject (x:xs) = fst x ++ ":" ++ show (snd x) ++ "," ++ createStringFromJObject xs

instance Eq JSON where
  JArray _ == JArray _ = True
  JBool _ == JBool _ = True
  JDouble _ == JDouble _ = True 
  JInteger _ == JInteger _ = True 
  JString _ == JString _ = True 
  JObject _ == JObject _= True 
  JNull == JNull = True 
  JNewObjectArray _ == JNewObjectArray _ = True
  _ == _ = False