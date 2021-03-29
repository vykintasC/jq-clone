module Jq.Filters where

import Jq.Json

data Filter = Identity |
  IndexObj {keys :: [(String, Bool)]} |
  IteratorAll |
  IteratorOpt {opt_indices :: [Int]}|
  Iterator {indices :: [Int]} |
  Slice {from_to :: (String, String)} |
  SliceOpt {from_to :: (String, String)} |
  Comma {filters_comma :: [Filter]} |
  Pipe {filters_pipe :: [Filter]} |
  Parentheses {inside :: Filter} |
  ObjectBuild {pairs :: [(Filter, Filter)]} |
  SimpleValue {just_string :: JSON} |
  ObjectBuildFromWords {word_list :: [String]} |
  ObjectList {filter :: Filter} |
  ObjectListCreation {filterList :: [Filter]} |
  FWord {str :: Filter}
  -- deriving (Show)

instance Show Filter where
  show (FWord s) = show s
  show Identity = "."
  show (IndexObj allKeys) = showKeysPairs allKeys
  show (Iterator xs) = "." ++ show xs
  show IteratorAll = ".[]"
  show (IteratorOpt []) = ".[]?"
  show (IteratorOpt xs) = "." ++ show xs ++ "?"
  show (Slice tuple) = ".[" ++ read (fst tuple) ++ ":" ++ read (snd tuple) ++ "]"
  show (SliceOpt tuple) = ".[" ++ show (fst tuple) ++ ":" ++ show (snd tuple) ++ "]?"
  show (Comma xs) = showFilterList xs
  show (Pipe xs) = showPipe xs
  show (Parentheses x) = "(" ++ show x ++ ")"
  show (ObjectBuild []) = "{}"
  show (ObjectBuild (x:xs)) = "{" ++ "\"" ++ show (fst x) ++ "\"" ++ ":" ++ theRest ++ "}"
    where 
      theRest = if null xs then one else more
        where 
          one = show (snd x)
          list = createStringFromJObject xs
          num = length list - 1
          more = one ++ "," ++ take num list
  show (SimpleValue value) = show value
  show (ObjectBuildFromWords xs) = "Find object keys: [" ++ showKeys xs ++ "]"
  show (ObjectList flt) = "[" ++ show flt ++ "]"
  show (ObjectListCreation xs) = showFilterList xs

createStringFromJObject :: [(Filter, Filter)] -> String
createStringFromJObject [] = ""
createStringFromJObject [x] = "\"" ++ show (fst x)  ++ "\"" ++ ":" ++ show (snd x) ++ ","
createStringFromJObject (x:xs) = "\"" ++ show (fst x) ++ "\"" ++ ":" ++ show (snd x) ++ "," ++ createStringFromJObject xs

showPipe :: [Filter] -> String 
showPipe [] = ""
showPipe [x] = show x
showPipe (x:xs) = show x ++ "|" ++ showPipe xs 

showFilterList :: [Filter] -> String 
showFilterList = concatMap show

showKeys :: [String] -> String 
showKeys [] = ""
showKeys [x] = x
showKeys (x:xs) = x ++ showKeys xs

showKeysPairs :: [(String, Bool)] -> String 
showKeysPairs [] = ""
showKeysPairs [(x,b)] = if b then x ++ "?" else x
showKeysPairs ((x,_):xs) = x ++ showKeysPairs xs

data Config = ConfigC {filters :: Filter}
  deriving (Show)
