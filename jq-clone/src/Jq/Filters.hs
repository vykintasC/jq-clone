module Jq.Filters where

data Filter = Identity |
  IndexObj {keys :: [String]} |
  IteratorAll |
  IteratorOpt |
  Iterator {indices :: [Int]} |
  Slice {from_to :: (Int, Int)} |
  Comma {filters_comma :: [Filter]} |
  Pipe {filters_pipe :: [Filter]} |
  Parentheses {inside :: Filter} |
  ObjectBuild {pairs :: [(String, Filter)]} |
  SimpleValue {just_string :: String} |
  ObjectBuildFromWords {word_list :: [String]}
  deriving (Show)

-- instance Show Filter where
--   show Identity = "."
--   show (IndexObj keys) = showKeys keys
--   show (Parentheses []) = ""
--   show (Parentheses (x:xs)) = "(" ++ show x ++ (showFilterList xs) ++ ")"
--   show (Iterator xs) = "." ++ show xs
--   show IteratorAll = ".[]"
--   show IteratorOpt = ".[]?"
--   show (Slice tuple) = ".[" ++ show (fst tuple) ++ ":" ++ show (snd tuple) ++ "]"
--   show (Comma xs) = showFilterList xs
--   show (Pipe xs) = showPipe xs

showPipe :: [Filter] -> String 
showPipe [] = ""
showPipe (x:[]) = show x
showPipe (x:xs) = show x ++ "|" ++ showPipe xs 

showFilterList :: [Filter] -> String 
showFilterList [] = ""
showFilterList (x:xs) = show x ++ showFilterList xs

showKeys :: [String] -> String 
showKeys [] = ""
showKeys (x:[]) = x
showKeys (x:xs) = x ++ showKeys xs

data Config = ConfigC {filters :: Filter}
  deriving (Show)
