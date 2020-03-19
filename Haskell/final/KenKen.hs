{-# OPTIONS_GHC -Wall -fprof-auto -fbreak-on-exception -rtsopts  #-}

module Main where

import Data.Char
import Data.Function
import Data.List
import Data.Ord
import System.Environment
import Misc


main :: IO()
main = do
    fName <- getArgs
    fcontent <- readFile (fName !! 0)
    let fLines = lines fcontent
    let dimens = read (fLines !! 0) :: Int
    putStrLn ("Board size: " ++ (show dimens))
    let noDimen = delAt fLines 0
    coords <- process noDimen 0 (length noDimen) dimens []
    let groups = groupValues coords
    let numGroup = sort (coordConvert groups [] 0)
    let groupStr = form (length groups) brFlag groups ""
    let byCol = listifyGrid groups 0 []
    let byRow = listifyGrid numGroup 0 []
    let rowChunk = chunkUp dimens byRow
    let colChunk = chunkUp dimens byCol
    putStrLn ("\n\n" ++ groupStr)
    putStrLn ("By row:\n" ++ (formGrid rowChunk 0 ""))
    putStrLn ("By col:\n" ++ (formGrid colChunk 0 ""))

    
validate :: [[Int]] -> Int -> Int -> [[Int]] -> [[Int]]
validate [] idx size accum = accum
validate seqs idx size accum 
    | ((li seqs)+1) == idx = accum
    | otherwise = validate seqs (idx + 1) size (accum ++ (checkRows chunks [] 0))
    where
        tmp = seqs !! idx
        chunks = chunkUp size tmp


checkRows :: [[Int]] -> [[Int]] -> Int -> [[Int]]
checkRows rows accum idx 
    | ((li rows)+1) == idx = accum
    | (length uni) == (length tmp) = checkRows rows (accum ++ [tmp]) (idx + 1) 
    | otherwise = checkRows rows (accum) (idx + 1) 
    where 
        tmp = rows !! idx
        uni = nub (tmp)

pickSomeGrids :: [[[Int]]] -> Int -> [[[[Int]]]] -> [[[[Int]]]]
pickSomeGrids chunks idx accum 
    | (li chunks) == idx = accum ++ [[picked]] 
    | otherwise = pickSomeGrids chunks (idx + 1) (accum ++ [[picked]])
    where picked = parse (chunks !! (idx)) 0 []

parse :: [[Int]] -> Int -> [[Int]] -> [[Int]]
parse line idx accum 
    | (li line) ==  idx = accum ++ picked
    | otherwise = parse line (idx + 1) (accum ++ picked)
    where picked = [pickNum (line !! (idx)) idx]

pickNum :: [Int] -> Int -> [Int]
pickNum list idx 
    | (li list) > idx = [list !! avail]
    | otherwise = [list !! avail]
    where avail = idx `rem` ((li list) + 1)  


formGrid :: [[[Int]]] -> Int -> String -> String
formGrid grid idx accum 
    | ((li grid) + 1) == idx = accum ++ "\n"
    | otherwise = formGrid grid (idx + 1) (accum ++ (show (grid !! idx)) ++ "\n")


formGrids :: [[[[Int]]]] -> Int -> String -> String
formGrids grid idx accum
    | ((li grid) + 1) == idx = accum ++ "\n"
    | otherwise = formGrids grid (idx + 1) (accum ++ ((formGrid (grid !! idx) 0 "")) )

process :: [String] -> Int -> Int -> Int -> [(String, Int)] -> IO [(String, Int)]
process line idx count size coords = do
    if count == 0
        then return coords
        else do
            let cLine = (line !! idx)
            let vCombos = findVals cLine size
            let reach = getCageGoal cLine
            pairIO <- possiblePairs (words cLine) vCombos reach
            let pairs = sort (nub(pairIO)) 
            putStrLn ("cage: " ++ (show (cLine !! 0)) ++ " op: " ++  (show (cLine !! (li cLine))) ++ 
                      " val: " ++ (show reach) ++ " combos: " ++ (show vCombos))
            process line (idx + 1) (count - 1) size (coords ++ pairs)



stringifyGrid :: [(String, [Int])] -> Int -> String -> String
stringifyGrid coordVals idx accum 
    | (((li coordVals) == idx) && ((length vals) == 1)) = (accum ++ sVal)
    | (((li coordVals) == idx) && ((length vals) /= 1)) = (accum ++ "*")
    | (length vals) == 1 = stringifyGrid coordVals (idx + 1) (accum ++ sVal) 
    | otherwise = stringifyGrid coordVals (idx + 1) (accum ++ "*")
    where
      tmp = coordVals !! idx 
      vals = snd tmp
      sVal = show (vals !! 0)

listifyGrid :: [(String, [Int])] -> Int -> [[Int]] -> [[Int]]
listifyGrid coordVals idx accum 
    | (li coordVals) == idx = accum ++ vals
    | otherwise = listifyGrid coordVals (idx + 1) (accum ++ vals)
    where vals = [snd (coordVals !! idx)]
 
    
coordConvert ::  [(String, [Int])] -> [(String, [Int])] -> Int -> [(String, [Int])]
coordConvert coords accum idx 
    | (li coords) == idx = accum ++ [(newCoord, (snd tmp))]
    | otherwise = coordConvert coords (accum ++ [(newCoord, (snd tmp))]) (idx + 1) 
    where
        tmp = coords !! idx 
        numCol = show ((indexOf ((fst tmp) !! 0) colNames 0) + 1)
        newCoord =  [(fst tmp) !! 1] ++ numCol


      
findVals ::  [Char] -> Int -> [[Int]] 
findVals list size
    | (op == '-') = doSub list size
    | op == '+' = doAdd list size
    | otherwise = [[(digitToInt op)]]
    where op = list !! (li list)

