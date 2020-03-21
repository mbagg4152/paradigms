{-# OPTIONS_GHC -Wall
    -fprof-auto
    -fbreak-on-exception
    -rtsopts #-}

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
      grs = form (length gr) brFlag gr ""
  putStrLn ("\n(row, col):\n" ++ grs)

  let listyGrid = listifyGrid gr 0 []
      fListy = formGrid (chunkUp dimens listyGrid) 0 ""
  putStrLn ("Displayed by row 1 .. end (preserves order of graphical grid):\n" ++ fListy)

  let adj = getAdjacents (2, 2) dimens
  putStrLn (show adj)

getPossible :: (Col, Row) -> [((Col, Row), [Possible])] -> [Possible]
getPossible (c, r) dList = snd (dList !! idx)
  where idx = pIndexOf (c, r) dList 0




equalPairs :: (Col, Row) -> (Col, Row) -> Bool
equalPairs (c1, r1) (c2, r2) = ((c1 == c2) && (r1 == r2))

getAdjacents :: ((Col, Row), [Possible]) -> Size -> [((Col, Row),[Possible])]
getAdjacents (c, r) s
  | (c, r) == (0, 0) = [right, below]
  | (c, r) == (0, (s - 1)) = [above, right]
  | (c, r) == ((s - 1), 0) = [left, below]
  | (c, r) == ((s - 1), (s - 1)) = [above, left]
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
  | (li gdlist) < (num) = (num `rem` 2)
  | otherwise = num
