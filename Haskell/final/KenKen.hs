{-# OPTIONS_GHC -Wall -fprof-auto -fbreak-on-exception -rtsopts  #-}

module Main where

import Data.Char
import Data.List
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
    let gr = coordPair groups [] 0 
    let grs = form (length gr) brFlag gr ""
    putStrLn ("\n\n" ++ grs)
    
    let listyGrid = listifyGrid groups 0 []
    let fListy = formGrid (chunkUp dimens listyGrid) 0 "" 
    putStrLn ("Order is by column a .. end:\n" ++ fListy)

formGrid :: [[[GData]]] -> Index -> String -> String
formGrid grid idx accum 
    | ((li grid) + 1) == idx = accum ++ "\n"
    | otherwise = formGrid grid (idx + 1) (accum ++ (show (grid !! idx)) ++ "\n")

process :: [Inputln] -> Index -> Count -> Size -> [(ColRow, GData)] -> IO [(ColRow, GData)]
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

listifyGrid :: [(ColRow, [GData])] -> Index -> [[GData]] -> [[GData]]
listifyGrid coordVals idx accum 
    | (li coordVals) == idx = accum ++ vals
    | otherwise = listifyGrid coordVals (idx + 1) (accum ++ vals)
    where vals = [snd (coordVals !! idx)]
 
      
findVals ::  Inputln -> Size -> [[Int]] 
findVals list size
    | (op == '-') = doSub list size
    | op == '+' = doAdd list size
    | otherwise = [[(digitToInt op)]]
    where op = list !! (li list)


coordPair ::  [(ColRow, [GData])] -> [((Col, Row), [GData])] -> Index -> [((Int, Int), [Int])]
coordPair coords accum idx 
    | (li coords) == idx = accum ++ [(newCoord, (snd tmp))]
    | otherwise = coordPair coords (accum ++ [(newCoord, (snd tmp))]) (idx + 1) 
    where
        tmp = coords !! idx
        loc = fst tmp 
        scol = loc !! 0
        irow = ((read [loc !! 1]) - 1)
        numCol = ((indexOf scol colNames 0))
        newCoord = (numCol, irow)
