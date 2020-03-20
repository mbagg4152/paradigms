{-# OPTIONS_GHC -Wall -fprof-auto -fbreak-on-exception -rtsopts  #-}

module Main where

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
    let gr =  sort (revCoordPair groups [] 0)
    let grs = form (length gr) brFlag gr ""
    putStrLn ("\n(row, col):\n" ++ grs)
    
    let listyGrid = listifyGrid gr 0 []
    let fListy = formGrid (chunkUp dimens listyGrid) 0 "" 
    putStrLn ("Displayed by row 1 .. end (preserves order of graphical grid):\n" ++ fListy)
    let n = getSurr (0,0) dimens
    putStrLn (show n)

rmn :: [((Col, Row), [GData])] -> Size -> [((Col, Row), [GData])]
rmn [((col, row), gdata)] dimen
   -- | len == 1 = upcut surrounding val 
    | otherwise = [((col, row), gdata)]
    where
        len = length gdata
        val = gdata !! 0 
        surrounding = getSurr (col,row) dimen



getgd :: (Col, Row) -> [((Col, Row), [GData])] -> [GData]
getgd (c,r) dList = snd (dList !! idx) 
    where idx = pIndexOf (c,r) dList 0



eqpair :: (Col, Row) -> (Col, Row) -> Bool
eqpair (c1,r1) (c2,r2) = ((c1 == c2) && (r1 == r2))



getSurr :: (Col, Row) -> Size -> [(Col, Row)]
getSurr (c,r) s
    | (c,r) == (0,0) = [rn,dn]
    | (c,r) == (0, (s- 1)) = [tn,rn]
    | (c,r) == ((s - 1), 0) = [ln,dn]
    | (c,r) == ((s - 1), (s - 1)) =  [tn,ln]    
    | otherwise = [rn,ln,dn,tn]
    where
        rn = ((c+1),r)
        ln = ((c-1),r)
        dn = (c,(r+1))
        tn = (c,(r-1))



process :: [Inputln] -> Index -> Count -> Size -> [(ColRow, GData)] -> IO [(ColRow, GData)]
process line idx count size coords = do
    if count == 0
        then return coords
        else do
            let cLine = (line !! idx)
            let vCombos = findVals cLine size
            let reach = getCageTarget cLine
            pairIO <- possiblePairs (words cLine) vCombos reach
            let pairs = sort (nub(pairIO)) 
            putStrLn ("cage: " ++ (show (cLine !! 0)) ++ " op: " ++  (show (cLine !! (li cLine))) ++ 
                      " val: " ++ (show reach) ++ " combos: " ++ (show vCombos))
            process line (idx + 1) (count - 1) size (coords ++ pairs)


mkList :: [((Col, Row), [GData])] -> [((Col, Row), [GData])] -> [((Col, Row), [GData])]
mkList [] accum = accum
mkList items accum 
    | len == 1 =  mkList lessItems (accum ++ [tmp]) 
    | otherwise = mkList lessItems accum
    where
        tmp = items !! 0
        vals =  snd tmp
        len = length vals
        lessItems = delAt items 0

chooseIndex :: [GData] -> Int -> Index
chooseIndex gdlist num 
    | (li gdlist) < (num) = (num `rem` 2)
    | otherwise = num
