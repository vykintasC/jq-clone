module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Debug.Trace


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile filt inp =
    do
        case filt of
            Identity -> return [inp]
            IndexObj strList -> do
                let filtered = filterIndexObj inp strList
                return filtered
            IteratorAll -> 
                case inp of
                    JArray values -> return values
                    JObject xs -> return (extractValues xs)
                    _ -> error (show inp ++ " is not a list or an object.") 
            IteratorOpt ->
                case inp of
                    JArray values -> return values
                    _ -> return []
            Iterator indices -> 
                case inp of
                    JArray values ->
                        do 
                            let filtered = filterIndices values indices
                            return filtered
                    _ -> error (show inp ++ " is not a list. You cannot index that.") 
            Slice (x, y) ->
                case inp of
                    JArray values ->
                        do 
                            let filtered = filterSlice values (x, y)
                            return filtered
                    _ -> error (show inp ++ " is not a list. You cannot slice that.")
            Comma xs -> return (compileList xs inp)
            Pipe xs -> return (compilePipe xs inp)
            Parentheses xs -> compile xs inp
            ObjectBuild xs -> return [JObject (buildObject xs inp)]
            SimpleValue _ -> return [JNull]
            ObjectBuildFromWords list -> return [JObject (getObjectsFromWords list inp)]

getObjectsFromWords :: [String] -> JSON -> [(String, JSON)]
getObjectsFromWords [] _ = []
getObjectsFromWords (x:xs) inp = res
    where   
        found = compile (IndexObj [x]) inp
        res = case found of
            Left _ -> (x, JNull) : getObjectsFromWords xs inp
            Right value -> (x, head value) : getObjectsFromWords xs inp
    

extractValues :: [(String, JSON)] -> [JSON]
extractValues [] = []
extractValues ((_,value):xs) = value : extractValues xs

buildObject :: [(String, Filter)] -> JSON -> [(String, JSON)]
buildObject [] _ = []
buildObject (x:xs) inp = obj 
    where
        key = fst x
        filt = snd x
        obj = case filt of
                SimpleValue str -> (key, JString str) : (buildObject xs inp)
                _ -> normalFiltered
                    where
                        applied = compile filt inp
                        normalFiltered = case applied of
                            Left _ -> error "Could not create an object."
                            Right value | null xs ->  [(key, JNewObjectArray value)]
                                        | otherwise -> (key, head value) : (buildObject xs inp)
                

compileList :: [Filter] -> JSON -> [JSON]
compileList [] _ = []
compileList (x:xs) inp = res
    where
        first = compile x inp
        res = case first of
            Left _ -> error "Comma could not be compiled."
            Right value -> value ++ compileList xs inp

compilePipe :: [Filter] -> JSON -> [JSON]
compilePipe (x:xs) inp = res
    where
        first = compile x inp
        res = case first of
            Left _ -> error "Pipe could not be compiled."
            Right (y:ys) | null xs -> y:ys
                         | otherwise -> compilePipe xs y
            Right [] -> []
compilePipe _ _ = []


filterIndexObj :: JSON -> [String] -> [JSON] 
filterIndexObj f [] = [f]
filterIndexObj (JObject (x:xs)) (s:ss) = res
    where
        object = getKeyObject (x:xs) s
        res = case object of
            [JObject (y:ys)] -> filterIndexObj (JObject (y:ys)) ss
            _ -> if null object then [JNull] else object 
filterIndexObj _ _ = [JNull]


getKeyObject :: [(String,JSON)] -> String -> [JSON]
getKeyObject _ [] = [JNull]
getKeyObject ((key, json):xs) s = if key == s then [json] else getKeyObject xs s
getKeyObject _ _ = []

filterIndices :: [JSON] -> [Int] -> [JSON]
filterIndices _ [] = []
filterIndices xs (idx:idxs) = res
    where
        itemList = take 1 (drop idx xs)
        res = if length itemList == 0 then JNull : filterIndices xs idxs else itemList ++ filterIndices xs idxs

filterSlice :: [JSON] -> (Int, Int) -> [JSON]
filterSlice xs (x , y) = if x >= y then [JArray []] else [JArray (take (y-x) (drop x xs))]

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
