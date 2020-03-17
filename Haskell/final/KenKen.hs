module Main where

import System.Environment
import System.IO
import Data.Char
import Data.Function
import Data.List


main = do
    fName <- getArgs
    fcontent <- readFile (fName !! 0)
    let fLines = lines fcontent
    
    let dimens = read (fLines !! 0) :: Int
    putStrLn ("Board size: " ++ (show dimens))
    
    let noDimen = delAt fLines 0
    coords <- procLines noDimen 0 (length noDimen) dimens []
    
    let sc = sort coords
    putStrLn "\n\n"
    print sc

findSubsets :: Int -> Int -> [[Int]]
findSubsets lim size = sub
    where
        legal = listConcat (getLegalVals lim []) size
        subsets = genLgSubset size legal
        sub = sort (uniqLists subsets 0)

procLines :: [String] -> Int -> Int -> Int -> [(String,Int)] -> IO [(String,Int)]
procLines lines idx count size coords = do
    if count == 0
        then return coords
        else do
            let cLine = (lines !! idx)
            let sol = findVals cLine size
            let allnum = map digitToInt (filter isDigit cLine)
            let reach = allnum !! (li allnum)
            pairIO <- possiblePairs (words cLine) sol
            let pairs = sort (nub(pairIO)) 
            let adCords = coords ++ pairs
            putStrLn ("cage size: " ++ (show (cLine !! 0)) ++ " op: " ++ 
                     (show (cLine !! (li cLine))) ++ " value: " ++ 
                     (show reach) ++ " combos: " ++ (show sol) )
            procLines lines (idx+1) (count-1) size adCords

possiblePairs :: [String] -> [[Int]] -> IO [(String,Int)]
possiblePairs str vals  = do
    let size = (read (str !! 0))
    let coord = str !! 1
    let val = (read (str !! 2))
    if size == 1
        then return [(coord, val)]
        else do
            let noSze = delAt str 0
            let noOp = delAt noSze (li noSze)
            let coordsOnly = delAt noOp (li noOp)
            coordPairs <- pairHelper (permutations coordsOnly) vals [] 0
            return (coordPairs)
        
pairHelper :: [[String]] -> [[Int]] -> [(String,Int)] -> Int -> IO [(String,Int)]
pairHelper str vals pairs idx = do
    let len = length vals
    if len == idx
        then return pairs 
        else do
            pairOne <- pairer str (vals!!idx) []  0
            pairHelper str vals (pairs ++ pairOne) (idx+1)

pairer :: [[String]] -> [Int] -> [(String,Int)] -> Int -> IO [(String,Int)]
pairer str vals pairs idx = do
    let len = length str
    if len == idx
        then return (pairs)
        else do
            pairer str vals (pairs ++ (zip (str !! idx) vals)) (idx+1)

findVals ::  [Char] -> Int -> [[Int]] 
findVals list size
    | op == '-' = doSub list size
    | op == '+' = doAdd list size
    | otherwise = [[(digitToInt op)]]
    where op = list !! (li list)

doSub :: [Char] -> Int -> [[Int]]
doSub chars size = list
    where 
        intList = map digitToInt (filter isDigit chars)
        res = intList!!(li intList)
        sets = findSubsets size (intList !!0) 
        list = subHelper res sets []
 
subHelper :: Int -> [[Int]] -> [[Int]] -> [[Int]]
subHelper reach [] succ = succ
subHelper reach sets succ = matches
    where
        good = succ ++ (lookAtPerms reach (permutations (sets!!0)) [] )
        matches = subHelper reach (delAt sets 0) good

lookAtPerms :: Int -> [[Int]] -> [[Int]] -> [[Int]]
lookAtPerms reach [] valid = valid
lookAtPerms reach perms valid
    | res == reach = lookAtPerms reach upPerm good
    | res /= reach = lookAtPerms reach upPerm valid
    | otherwise = lookAtPerms reach upPerm valid
    where
        cur = perms!!0    
        shortened = delAt (delAt cur 0) 0
        upPerm = filter (/= cur) perms
        good = valid ++ [cur]
        res = abs (minus ((cur!!0) - (cur!!1)) 0 shortened (length shortened)) 
    
minus :: Int -> Int -> [Int] -> Int -> Int
minus total idx list len
    | len == 0 = total
    | len == 1 = total - (list!!(len-1))
    | otherwise = minus (total - (list!!idx)) (idx+1) list (len-1)       

doAdd :: [Char] -> Int -> [[Int]]
doAdd chars size = list
    where 
        intList = map digitToInt (filter isDigit chars)
        res = intList !! (li intList)
        sets = findSubsets size (intList!!0)
        list = addHelper sets [] res 0

addHelper :: [[Int]] -> [[Int]] -> Int -> Int -> [[Int]]
addHelper list succ reach idx 
    | (li list) == idx = succ
    | summed == reach = addHelper list (succ ++ [tmp]) reach (idx + 1)
    | summed /= reach = addHelper list succ reach (idx + 1)
    where 
        tmp = list !! idx
        summed = sum tmp

getLegalVals :: Int -> [Int] -> [Int]
getLegalVals 0 intList = intList
getLegalVals size intList = sort ([size] ++ (getLegalVals (size -1) intList))

genLgSubset :: Int -> [Int] -> [[Int]]
genLgSubset 0 _ = [[]]
genLgSubset _ [] = []
genLgSubset lim (cur:next) = 
    [cur : subs | subs <- genLgSubset (lim - 1) next] ++ genLgSubset lim next 

listConcat :: [Int] -> Int -> [Int]
listConcat list times = sort (concat (replicate times list))

uniqLists :: Ord a => Eq a => [[a]] -> Int -> [[a]]
uniqLists [] idx = []
uniqLists list idx
    | idx == ((length list)-1) = list
    | idx /= ((length list)-1) = uniqueLol
    | otherwise = []
    where
        tmpList = sort(list !! idx)
        shortened = filter (/= tmpList) list
        newList = [tmpList] ++ shortened
        uniqueLol = uniqLists newList (idx + 1)

delAt :: [a] -> Int -> [a]
delAt list idx = take idx list ++ drop (idx + 1) list

li :: [a] -> Int
li str = (length str) - 1