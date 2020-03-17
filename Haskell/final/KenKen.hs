module Main where

import System.Environment
import System.IO
import Data.Char
import Data.Function
import Data.List


main = do
    fiName <- getArgs
    fiContents <- readFile (fiName !! 0)
    let fiLines = lines fiContents

    let dimen = read (fiLines !! 0) :: Int
    putStrLn ("Board size: " ++ (show dimen))

    let noDimens = delAt fiLines 0
    coords <- procLines noDimens 0 (length noDimens) dimen []
    
    let sc = sort coords
    print sc

findSubsets :: Int -> Int -> [[Int]]
findSubsets lim size = subsets
    where
        legal = listConcat (getLegalVals lim []) size
        subs = genLgSubset size legal
        subsets = sort (uniqLists subsets 0)

procLines :: [String] -> Int -> Int -> Int -> [(String, Int)] -> IO [(String,Int)]
procLines lines idx count size coords = do
    if count == 0
        then return coords
        else do
            let thisLine = (lines !! idx)
            let possible = findVals thisLine size
            let numsOnly = map digitToInt (filter isDigit thisLine)
            let goal = numsOnly !! (li numsOnly)
            
            ioPair <- possPairs (words thisLine) possible
            let pairs = sort (nub ioPair) 
            
            putStrLn ("cage size: " ++ (show (thisLine !! 0)) ++ " op: " ++  (show (thisLine !! (li thisLine))) ++ 
                     " value: " ++ (show goal) ++ " combos: " ++ (show possible) ++ "\n")
            procLines lines (idx + 1) (count - 1) size (coords ++ pairs)

possPairs :: [String] -> [[Int]] -> IO [(String, Int)]
possPairs str vals  = do
    let size = (read (str !! 0))
    let coord = str !! 1
    let val = (read (str !! 2))
    if size == 1
        then return [(coord, val)]
        else do
            let noSize = delAt str 0
            let noOp = delAt noSize (li noSize)
            let coordsOnly = delAt noOp (li noOp)
            coordPairs <- outerPairHelper (permutations coordsOnly) vals [] 0
            return (coordPairs)
        
outerPairHelper :: [[String]] -> [[Int]] -> [(String, Int)] -> Int -> IO [(String, Int)]
outerPairHelper str vals pairs idx = do
    let len = length vals
    if len == idx
        then return pairs 
        else do
            pairOne <- innerPairHelper str (vals!!idx) []  0
            outerPairHelper str vals (pairs ++ pairOne) (idx + 1)

innerPairHelper :: [[String]] -> [Int] -> [(String,Int)] -> Int -> IO [(String,Int)]
innerPairHelper str vals pairs idx = do
    let len = length str
    if len == idx
        then return (pairs)
        else do
            innerPairHelper str vals (pairs ++ (zip (str !! idx) vals)) (idx + 1)

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
        result = intList !! (li intList)
        sets = findSubsets size (intList !!0) 
        list = subHelper result sets []
 
subHelper :: Int -> [[Int]] -> [[Int]] -> [[Int]]
subHelper goal [] accum = accum
subHelper goal sets accum = matches
    where
        valid = accum ++ (checkPerms goal (permutations (sets !! 0)) [])
        matches = subHelper goal (delAt sets 0) valid

checkPerms :: Int -> [[Int]] -> [[Int]] -> [[Int]]
checkPerms goal [] accum  = accum 
checkPerms goal perms accum 
    | result == goal = checkPerms goal choppedPerms (accum  ++ [cur])
    | result /= goal = checkPerms goal choppedPerms accum 
    | otherwise = checkPerms goal choppedPerms accum 
    where
        cur = perms !! 0    
        chopped = delAt (delAt cur 0) 0
        choppedPerms = filter (/= cur) perms
        result = abs (minus ((cur !! 0) - (cur !! 1)) 0 chopped (length chopped)) 
    
minus :: Int -> Int -> [Int] -> Int -> Int
minus total idx list len
    | len == 0 = total
    | len == 1 = total - (list !! (len - 1))
    | otherwise = minus (total - (list !! idx)) (idx + 1) list (len - 1)       

doAdd :: [Char] -> Int -> [[Int]]
doAdd chars size = list
    where 
        intList = map digitToInt (filter isDigit chars)
        result = intList!!((length intList)-1)
        sets = findSubsets size (intList!!0)
        list = addHelper sets [] result 0

addHelper :: [[Int]] -> [[Int]] -> Int -> Int -> [[Int]]
addHelper list accum goal idx 
    | (li list) == idx = accum
    | summed == goal = addHelper list (accum ++ [tmp]) goal (idx + 1)
    | summed /= goal = addHelper list accum goal (idx + 1)
    where 
        tmp = list !! idx
        summed = sum tmp

getLegalVals :: Int -> [Int] -> [Int]
getLegalVals 0 intList = intList
getLegalVals size intList = sort ([size] ++ (getLegalVals (size - 1) intList))

genLgSubset :: Int -> [Int] -> [[Int]]
genLgSubset 0 _ = [[]]
genLgSubset _ [] = []
genLgSubset lim (cur : next) = [cur : subs | subs <- genLgSubset (lim - 1) next] ++ genLgSubset lim next 


uniqLists :: Ord a => Eq a => [[a]] -> Int -> [[a]]
uniqLists [] idx = []
uniqLists list idx
    | idx == (li list) = list
    | idx /= (li list) = uniList
    | otherwise = []
    where
        inner = sort (list !! idx)
        chopped = filter (/= inner) list
        uniList = uniqLists ([inner] ++ chopped) (idx + 1)


listConcat :: [Int] -> Int -> [Int]
listConcat list times = sort (concat (replicate times list))

delAt :: [a] -> Int -> [a]
delAt list idx = take idx list ++ drop (idx + 1) list

li :: [a] -> Int
li str = (length str) - 1