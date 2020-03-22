{-# OPTIONS_GHC -Wall
    -fprof-auto -fprof-cafs
    -fbreak-on-exception
    -rtsopts
     #-}
{-# LANGUAGE ExplicitForAll #-}

  module Main where

  import           Data.Char
  import           Data.Function
  import           Data.List
  import           Data.Ord
  import           System.Environment

  brFlag :: Int
  brFlag = 3

  colNames :: [Char]
  colNames = "abcdef"

  main :: IO ()
  main = do
    fName <- getArgs
    fcontent <- readFile (fName !! 0)
    let fLines = lines fcontent
        dimens = read (fLines !! 0) :: Int
    putStrLn ("Board size: " ++ (show dimens))

    coords <- process (tail fLines) 0 (length (tail fLines)) dimens []
    let groups = groupValues coords
        revGrp = sort (map revCoordPair groups)
        normGrp= sort (map coordPair groups)
        revGStr = form (length revGrp) brFlag revGrp ""
        normGStr = form (length normGrp) brFlag normGrp ""
    putStrLn ({- "\n(row, col):\n" ++ revGStr ++ -} "\n\n(col,row):\n" ++ normGStr)

    let listyGrid = listifyGrid revGrp 0 []
        smList = shortest listyGrid
        fListy = formGrid (chunkUp dimens listyGrid) 0 ""
    putStrLn ("\n"++ (show smList) ++ "\n")
    let checkData = checking 0 normGrp dimens
        chDataStr = form (length checkData) brFlag checkData ""
    putStrLn (show checkData)

  checking :: Index -> [((Col, Row), [Possible])] -> Size -> [((Col, Row), [Possible])]
  checking idx gridData dimens = removeFromAdjacent (zip adj accord) picked
      where allCoord = fst (unzip gridData)
            allVals = snd (unzip gridData)
            shortList = shortest allVals
            smIdx = indexOf shortList allVals  0
            adj = getAdjacent (allCoord !! smIdx) (dimens)
            accord = map (according gridData) adj
            picked = chooseIndex shortList 1
            gridDataLess = delAt gridData smIdx

  removeFromAdjacent :: [((Col, Row), [Possible])] -> Target -> [((Col, Row), [Possible])]
  removeFromAdjacent dat target = zip coord filt
      where coord = fst (unzip dat)
            val = snd (unzip dat)
            filt = map (filter (/= target)) val

  according :: [((Col, Row), [Possible])] -> (Col, Row) -> [Possible]
  according dat (ic, ir) = snd (dat !! (indexOfPair (ic, ir) dat 0))

  sameLengths :: [[a]] -> Bool
  sameLengths []     = True
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

  getAdjacent :: (Col, Row) -> Size -> [(Col, Row)]
  getAdjacent (c, r) s
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

  possiblePairs :: [PosStr] -> [[Possible]] -> Target -> IO [(PosStr, Possible)]
  possiblePairs str vals target = do
    let size = (read (head str)) :: Int
    if size == 1
      then return [((str !! 1), target)]
      else do
        coordPairs <- outerPairer (permutations (init (init (tail str)))) vals [] 0
        return (coordPairs)

  outerPairer :: [[PosStr]] -> [[Possible]] -> [(PosStr, Possible)] -> Index -> IO [(PosStr, Possible)]
  outerPairer str vals accum idx = do
    if (length vals) == idx
      then return accum
      else do
        pairOne <- innerPairer str (vals !! idx) [] 0
        outerPairer str vals (accum ++ pairOne) (idx + 1)

  innerPairer :: [[PosStr]] -> [Possible] -> [(PosStr, Int)] -> Index -> IO [(PosStr, Int)]
  innerPairer str vals accum idx = do
    if (length str) == idx
      then return (accum)
      else do innerPairer str vals (accum ++ (zip (str !! idx) vals)) (idx + 1)

  doSub :: String -> Size -> [[Int]]
  doSub chars maxNum
    | cage > 3 = subHelper (getCageTarget chars) (findSubsets maxNum (cage-1)) []
    | otherwise = subHelper (getCageTarget chars) (findSubsets maxNum cage) []
    where cage = (digitToInt (head (filter isDigit chars)))

  subHelper :: Target -> [[Int]] -> [[Possible]] -> [[Possible]]
  subHelper target [] accum = accum
  subHelper target sets accum = subHelper target (delAt sets 0) updated
    where updated = accum ++ (validate target ([(head sets)]) [])

  validate :: Target -> [[Int]] -> [[Possible]] -> [[Possible]]
  validate target [] valid = valid
  validate target perms valid
    | total == target = validate target lessPerms (valid ++ [cur])
    | otherwise = validate target lessPerms valid
    where cur = head perms
          lessPerms = filter (/= cur) perms
          total = abs (foldl (-) (head cur) (tail cur))

  doAdd :: String -> Size -> [[Possible]]
  doAdd chars maxNum
    | cage > 3 = addHelper (findSubsets maxNum (cage)) [] (getCageTarget chars) 0
    | otherwise = addHelper (findSubsets maxNum cage) [] (getCageTarget chars) 0
    where cage = (digitToInt (head (filter isDigit chars)))

  addHelper :: [[Int]] -> [[Possible]] -> Target -> Index -> [[Possible]]
  addHelper [] accum target idx = accum
  addHelper list accum target idx
    | (li list) == idx = accum
    | ((sum (list !! idx)) == target) && ((li list) /= idx) = addHelper list (accum ++ [(list !! idx)]) target (idx + 1)
    | ((sum (list !! idx)) /= target) && ((li list) /= idx) = addHelper list accum target (idx + 1)

  findSubsets :: Size -> Size -> [[Possible]]
  findSubsets maxNum cageSize = sort (uniqLists (setGen cageSize legal) 0)
    where legal = repConcat (getLegalVals maxNum) cageSize

  indexOf :: Eq a => a -> [a] -> Index -> Index
  indexOf lmnt arr idx
    | lmnt == (arr !! idx) = idx
    | lmnt /= (arr !! idx) = indexOf lmnt arr (idx + 1)
    | otherwise = -1

  indexOfPair :: Eq a => (a, a) -> [((a, a), [a])] -> Index -> Index
  indexOfPair lmnt arr idx
    | lmnt == (fst (arr !! idx)) = idx
    | lmnt /= (fst (arr !! idx)) = indexOfPair lmnt arr (idx + 1)
    | otherwise = -1

  getCageTarget :: Inputln -> Target
  getCageTarget str
    | (read (head wList)) == 1 = read (last wList)
    | str == "" = 0
    | otherwise = target
    where wList = words str
          target = read (last (init wList)) :: Int

  getLegalVals :: Size -> [Int]
  getLegalVals 0    = []
  getLegalVals size = sort ([size] ++ (getLegalVals (size - 1)))

  makeGrid :: Size -> Count -> String -> String
  makeGrid size count accum
    | (count `quot` 2) == size = accum ++ (makeGridLines size "")
    | (count `rem` 2) == 0 = makeGrid size (count + 1) (accum ++ (makeGridLines size ""))
    | (count `rem` 2) == 1 = makeGrid size (count + 1) (accum ++ (makeGridSpaces size ""))
    | otherwise = ""

  makeGridLines :: Size -> String -> String
  makeGridLines size accum
    | size == 0 = accum ++ "+\n"
    | otherwise = makeGridLines (size - 1) (accum ++ "+-----")

  makeGridSpaces :: Size -> String -> String
  makeGridSpaces size accum
    | size == 0 = accum ++ "|\n"
    | otherwise = makeGridSpaces (size - 1) (accum ++ "|     ")

  setGen :: Size -> [Int] -> [[Int]]
  setGen 0 _ = [[]]
  setGen _ [] = []
  setGen lim (cur : next) = [cur : subs | subs <- setGen (lim - 1) next] ++ setGen lim next

  repConcat :: [Int] -> Count -> [Int]
  repConcat list times
    | times > 3 = sort (concat (replicate (times-2) list))
    | otherwise = sort (concat (replicate times list))

  uniqLists :: Ord a => Eq a => [[a]] -> Index -> [[a]]
  uniqLists [] idx = []
  uniqLists list idx
    | idx < (li list) = uniqLists ([tmpList] ++ (filter (/= tmpList) list)) (idx + 1)
    | otherwise = list
    where tmpList = sort (list !! idx)

  groupValues :: [(PosStr, Possible)] -> [(PosStr, [Possible])]
  groupValues = map (\list -> (fst . head $ list, map snd list)) . groupBy ((==) `on` fst) . sortBy (comparing fst)

  chunkUp :: Index -> [a] -> [[a]]
  chunkUp _ [] = []
  chunkUp count ns =
    let (os, ps) = splitAt count ns
      in os : chunkUp count ps

  delAt :: [a] -> Index -> [a]
  delAt list idx = take idx list ++ drop (idx + 1) list

  li :: [a] -> Int
  li str = (length str) - 1

  form :: (Read a, Read b, Show a, Show b) => Len -> Count -> [(a, b)] -> String -> String
  form 0 nlf pairs accum = accum
  form len nlf pairs accum = formed
    where coord = show (fst (head pairs))
          num = show (snd (head pairs))
          out = accum ++ "" ++ coord ++ " --> " ++ num ++ " " ++ (fst (endsep nlf))
          cut = drop 1 pairs
          formed = form (length cut) (snd (endsep nlf)) cut out

  endsep :: Count -> (String, Int)
  endsep 1   = ("\n", brFlag)
  endsep cnt = ("\t", (cnt - 1))

  formGrid :: [[[Possible]]] -> Index -> String -> String
  formGrid grid idx accum
    | ((li grid) + 1) == idx = accum ++ "\n"
    | otherwise = formGrid grid (idx + 1) (accum ++ (show (grid !! idx)) ++ "\n")

  listifyGrid :: [(a, [Possible])] -> Index -> [[Possible]] -> [[Possible]]
  listifyGrid coordVals idx accum
    | (li coordVals) == idx = accum ++ vals
    | otherwise = listifyGrid coordVals (idx + 1) (accum ++ vals)
    where
      vals = [snd (coordVals !! idx)]

  findVals :: Inputln -> Size -> [[Int]]
  findVals list size
    | (last list) == '-' = doSub list size
    | (last list) == '+' = doAdd list size
    | otherwise = [[(digitToInt (last list))]]

  uniqGrid :: Ord a => Eq a => [((a, a), [a])] -> Index -> [((a, a), [a])]
  uniqGrid list idx
    | idx < (li list) =  uniqGrid ([((fst tmp), (snd tmp))] ++ shortened) (idx + 1)
    | otherwise = list
    where tmp = list !! idx
          shortened = filter (/= tmp) list

  coordPair :: (PosStr, [Possible]) -> ((Col, Row), [Possible])
  coordPair coords = ((numCol, irow), (snd coords))
    where
      irow = ((read [(fst coords) !! 1]) - 1)
      numCol = ((indexOf (head (fst coords)) colNames 0))

  revCoordPair :: (PosStr, [Possible]) -> ((Row, Col), [Possible])
  revCoordPair coords = ((irow, numCol), (snd coords))
    where
      irow = ((read [(fst coords) !! 1]) - 1)
      numCol = ((indexOf (head (fst coords)) colNames 0))

  type Count = Int
  type Col = Int
  type Index = Int
  type Possible = Int
  type Len = Int
  type Row = Int
  type Size = Int
  type Target = Int
  type PosStr = String
  type Inputln = String
