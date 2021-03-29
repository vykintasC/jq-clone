module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import Debug.Trace

type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile filt inp =
    do
        -- Pattern match on filter
        case filt of
            Identity -> return [inp] -- Return everything
            IndexObj strList -> do 
                let filtered = filterIndexObj inp strList
                return filtered -- Return a filtered JSON on indices .foo
            IteratorAll -> 
                case inp of
                    JArray values ->return values -- If it is an array then iterate over the array values
                    JObject xs -> return (extractValues xs) -- If it is an object, iterate over the object values
                    _ -> return [JNull]
            IteratorOpt idxs ->
                case inp of
                    JArray values | null idxs -> return values -- if indices' list is empty then iterate over all of them
                                  | otherwise -> do 
                                        let filtered = filterIndices values idxs -- If input is a list with indices then we can pick them out
                                        return filtered
                    JObject xs -> return (extractValues xs) -- Optionally iterate if it's an object
                    _ -> return []
            Iterator allIndices -> 
                case inp of
                    JArray values ->
                        do 
                            let filtered = filterIndices values allIndices -- If input is a list then we can pick out indices
                            return filtered
                    _ -> error (show inp ++ " is not a list. You cannot index that.") 
            Slice (x, y) ->
                case inp of
                    JArray values -> return (filterSlice values (x, y))
                    JString strng -> return (filterString strng (x, y))
                    _ -> error (show inp ++ " is not a list. You cannot slice that.")
            SliceOpt (x, y) -> 
                case inp of
                    JArray values -> return (filterSlice values (x, y))-- If input is a list we can slice it in the interval
                    JString strng -> return (filterString strng (x, y))
                    JNull -> return [JNull]
                    _ -> return []
            Comma xs -> return (compileList xs inp) 
            Pipe xs -> return (compilePipe xs inp)
            Parentheses xs -> compile xs inp
            ObjectBuild xs -> return [JObject (buildObject xs inp)]
            SimpleValue json -> return [json]
            ObjectBuildFromWords list -> return [JObject (getObjectsFromWords list inp)]
            ObjectList filt -> case inp of
                    JArray _ ->
                        case filt of
                            Pipe (x:xs) -> do
                                let filtered = compile x inp -- If input is a list then we can pick out indices
                                case filtered of
                                    Left _ -> error "Cannot filter the list input object"
                                    Right value -> return [JArray (makeObjectList xs value)]  
                            _ -> return [JArray (makeObjectList [filt] [inp])]
                    _ -> error (show inp ++ " is not a list. You cannot index that.") 
            ObjectListCreation list -> return [JArray (objectListCreation list inp)]
            FWord flt -> case flt of 
                SimpleValue (JString s) ->
                    do
                        let compiled = compile (IndexObj [(s, False)]) inp
                        case compiled of
                            Left _ -> error (show flt ++ "  has to be a string.")
                            Right value -> return value
                _ -> error (show flt ++ "  has to be a string.")


objectListCreation :: [Filter] -> JSON -> [JSON]
objectListCreation [] _ = []
objectListCreation (x:xs) inp = result
    where
        result = case x of
            SimpleValue json -> json : objectListCreation xs inp
            _ -> normalFiltered
                    where
                        compiled = compile x inp
                        normalFiltered = case compiled of
                                Left _ -> error "Could not create an object."
                                Right value -> value ++ objectListCreation xs inp
        
-- List creation with filters
makeObjectList :: [Filter] -> [JSON] -> [JSON]
makeObjectList [] value = value
makeObjectList _ [] = []
makeObjectList xs (y:ys) = compilePipe xs y ++ makeObjectList xs ys

-- {user} -> build object and get a key .user from the input
getObjectsFromWords :: [String] -> JSON -> [(String, JSON)]
getObjectsFromWords [] _ = []
getObjectsFromWords (x:xs) inp = res
    where   
        found = compile (IndexObj [(x,False)]) inp
        res = case found of
            Left _ -> (x, JNull) : getObjectsFromWords xs inp
            Right value -> (x, head value) : getObjectsFromWords xs inp
    
-- Extract values from the JObject
extractValues :: [(String, JSON)] -> [JSON]
extractValues [] = []
extractValues ((_,value):xs) = value : extractValues xs

-- Build object from key value pairs
buildObject :: [(Filter, Filter)] -> JSON -> [(String, JSON)]
buildObject [] _ = []
buildObject (x:xs) inp = obj 
    where
        key = case fst x of 
                FWord (SimpleValue (JString s)) -> s
                SimpleValue (JString s) -> s
                _ -> normalFiltered
                    where
                        applied = compile (fst x) inp -- apply the filter to the input
                        normalFiltered = case applied of
                            Left _ -> error "Could not create an object."
                            Right [JString s] -> s
                            Right value -> error (show value ++ " cannot be converted into a key.")
        filt = snd x
        obj = case filt of
                SimpleValue s -> (key, s) : buildObject xs inp -- If it's something like "x":true then it's just a simple value, no need to filter on the input.
                _ -> normalFiltered
                    where
                        applied = compile filt inp -- apply the filter to the input
                        normalFiltered = case applied of
                            Left _ -> error "Could not create an object."
                            Right value | null xs ->  [(key, JNewObjectArray value)] -- At this point only a list left so return all of it.
                                        | otherwise -> (key, head value) : buildObject xs inp
                
-- Compile every filter which is separated by comma and put the results in one list.
compileList :: [Filter] -> JSON -> [JSON]
compileList [] _ = []
compileList (x:xs) inp = res
    where
        first = compile x inp
        res = case first of
            Left _ -> error "Comma could not be compiled."
            Right value -> value ++ compileList xs inp

-- Compile pipe, and recursively make the input smaller and pass it on to the next filter in the pipe sequence.
compilePipe :: [Filter] -> JSON -> [JSON]
compilePipe [IndexObj _] (JObject []) = [JNull]
compilePipe [Pipe _] (JObject []) = [JNull]
compilePipe (x:xs) inp = res
    where
        first = compile x inp
        res = case first of
            Left _ -> error "Pipe could not be compiled."
            Right (y:ys) | null xs -> y:ys
                         | otherwise -> compilePipe xs y
            Right [] -> []
compilePipe _ _ = []

-- From the input only pick the values for which the keys are in the [(String, Bool)].
filterIndexObj :: JSON -> [(String, Bool)] -> [JSON] 
filterIndexObj (JObject []) _ = [JNull]
filterIndexObj f [] = [f]
filterIndexObj (JObject (x:xs)) ((s,optional):ss) = res
    where
        object = getKeyObject (x:xs) s optional
        res = case object of
            [JObject (y:ys)] -> filterIndexObj (JObject (y:ys)) ss
            _ -> if null object then [JNull] else object 
filterIndexObj _ _ = []

-- Helper function to get the value of the object for the given key. Bool is for optionality
getKeyObject :: [(String,JSON)] -> String -> Bool -> [JSON]
getKeyObject [] _ optional = if optional then [JEmpty] else [JNull]
getKeyObject ((key, json):xs) s optional = if key == s then [json] else getKeyObject xs s optional

-- Take only the indices of the array which are given in [Int]
filterIndices :: [JSON] -> [Int] -> [JSON]
filterIndices _ [] = []
filterIndices xs (idx:idxs) = res
    where
        new_idx = if idx < 0 then length xs + idx else idx
        itemList = take 1 (drop new_idx xs)
        res = if null itemList then JNull : filterIndices xs idxs else itemList ++ filterIndices xs idxs

-- Slice the array
filterSlice :: [JSON] -> (String, String) -> [JSON]
filterSlice xs (s, "") = [JArray (drop (read s) xs)]
filterSlice xs ("", s) = [JArray (take (read s) xs)]
filterSlice xs (x , y) = res
    where
        new_x = if read x < 0 then 
                    if length xs + read x < 0 then 0 else length xs + read x
                else read x
        new_y = if read y < 0 then length xs + read y else read y 
        res = if new_x >= new_y then [JArray []] else [JArray (take (new_y-new_x) (drop new_x xs))]

-- Slice the string
filterString :: String -> (String, String) -> [JSON]
filterString xs (x , y) = if read x++"" >= read y++"" then [JString ""] else [JString (take (read y-read x) (drop (read x) xs))]

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
