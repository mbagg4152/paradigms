{-# OPTIONS_GHC -Wall
    -fprof-auto -fprof-cafs
    -fbreak-on-exception
    -rtsopts
     #-}

module Main where

import           Data.List
import           Misc
import           System.Environment

main :: IO ()
main = do
  fName <- getArgs
  fcontent <- readFile (fName !! 0)
  let fLines = lines fcontent
      dimens = read (fLines !! 0) :: Int
  putStrLn ("Board size: " ++ (show dimens))

  coords <- process (tail fLines) 0 (length (tail fLines)) dimens []
  let groups = groupValues coords
      gr = sort (map revCoordPair groups)
      rgr= sort (map coordPair groups)
      grs = form (length gr) brFlag gr ""
      rgrs = form (length rgr) brFlag rgr ""
  putStrLn ({- "\n(row, col):\n" ++ grs ++ -} "\n\n(col,row):\n" ++ rgrs)

  let listyGrid = listifyGrid gr 0 []
      smList = shortest listyGrid
      fListy = formGrid (chunkUp dimens listyGrid) 0 ""
  --putStrLn ("\nDisplayed by row 1 .. end (preserves order of graphical grid):\n" ++ fListy)
  putStrLn ("\n"++ (show smList) ++ "\n")
  let chk = checking 0 rgr dimens
      schk = form (length chk) brFlag chk ""
  putStrLn (show chk)


checking :: Index -> [((Col, Row), [Possible])] -> Size -> [((Col, Row), [Possible])]
checking idx dat dimens = r 
    where allCoord = fst (unzip dat)
          allVals = snd (unzip dat)
          small = shortest allVals
          vIdx = indexOf small allVals  0
          adj = getAdjacents (allCoord !! vIdx) (dimens)
          accord = map (according dat) adj
          sel = chooseIndex small 1
          sdat = delAt dat vIdx
          r = rmn (zip adj accord) sel


rmn :: [((Col, Row), [Possible])] -> Target -> [((Col, Row), [Possible])]
rmn dat target = zip coord filt
    where coord = fst (unzip dat)
          val = snd (unzip dat)
          filt = map (filter (/= target)) val
 


                

according :: [((Col, Row), [Possible])] -> (Col, Row) -> [Possible]
according dat (ic, ir) = snd (dat !! (indexOfPair (ic, ir) dat 0))


sameLengths :: [[a]] -> Bool
sameLengths [] = True
sameLengths (x:xs) = all (\y -> length y == length x) xs

getPossible :: (Col, Row) -> [((Col, Row), [Possible])] -> [Possible]
getPossible (c, r) dList = snd (dList !! idx)
  where idx = indexOfPair (c, r) dList 0

shortest :: [[a]] -> [a]
shortest [] = []
shortest [x] = x
shortest (x:xs) = let s = shortest xs
                  in if length s < length x then s else x




equalPairs :: (Col, Row) -> (Col, Row) -> Bool
equalPairs (c1, r1) (c2, r2) = ((c1 == c2) && (r1 == r2))

getAdjacents :: (Col, Row) -> Size -> [(Col, Row)]
getAdjacents (c, r) s
  | (c, r) == (0, 0) = [right, below]
  | (c, r) == (0, (s - 1)) = [above, right]
  | (c, r) == ((s - 1), 0) = [left, below]
  | (c, r) == ((s - 1), (s - 1)) = [above, left]
  | c == (s - 1) = [above, left, below]
  | c == 0 = [above, right, below]
  | r == (s - 1) = [above, left, right]
  | r == 0 = [below, left, right] 
  | otherwise = [right, left, below, above]
  where right = ((c + 1), r)
        left = ((c - 1), r)
        below = (c, (r + 1))
        above = (c, (r - 1))

process :: [Inputln] -> Index -> Count -> Size -> [(PosStr, Possible)] -> IO [(PosStr, Possible)]
process line idx count size coords = do
  if count == 0
    then return coords
    else do
      let cLine = (line !! idx)
          vCombos = findVals cLine size
          reach = getCageTarget cLine
      pairIO <- possiblePairs (words cLine) vCombos reach
      let pairs = sort (nub (pairIO))
      putStrLn ( "cage: " ++ (show (head cLine)) ++ " op: " ++ (show (last cLine)) ++
                 " val: " ++ (show reach) ++ " combos: " ++ (show vCombos))
      process line (idx + 1) (count - 1) size (coords ++ pairs)

mkList :: [((Col, Row), [Possible])] -> [((Col, Row), [Possible])] -> [((Col, Row), [Possible])]
mkList [] accum = accum
mkList items accum
  | (length vals) == 1 = mkList (tail items) (accum ++ [(head items)])
  | otherwise = mkList (tail items) accum
  where vals = snd (head items)

chooseIndex :: [Possible] -> Int -> Index
chooseIndex gdlist num
  | (li gdlist) < (num) = (num `rem` 3)
  | otherwise = num
