{-# OPTIONS_GHC -Wall
    -fprof-auto -fprof-cafs
    -fbreak-on-exception
    -rtsopts
     #-}
{-# LANGUAGE ExplicitForAll #-}

  module Main where

  import Data.Char
  import Data.Function
  import Data.List
  import Data.Ord
  import System.Environment
  import System.Random
  import Debug.Trace

  brFlag :: Int
  brFlag = 4

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
        revGrp = sort (map rowColPair groups)
        normGrp = sort (map colRowPair groups)
        --revGStr = form (length revGrp) brFlag revGrp ""
        normGStr = form (length normGrp) brFlag normGrp ""
    putStrLn ("\n\n(col,row):\n" ++ normGStr)

    --let --listyGrid = listifyGrid revGrp 0 []
        --smList = shortest listyGrid
        --fListy = formGrid (chunkUp dimens listyGrid) 0 ""
    --putStrLn ("\n"++ (show smList) ++ "\n")
    let byLen = sortBy (comparing (length . snd)) normGrp
    --putStrLn ((form (length byLen) brFlag byLen "") ++ "\n\n")
   
    checkData <- runner byLen dimens
    --walked <- walkPaths 0 0 byLen dimens byLen 
    let chDataList = chunkUp dimens (listifyGrid (sort checkData) 0 [])
        chDataStr = formGrid chDataList 0 ""

    putStrLn (chDataStr ++  "\n") 
  
  runner :: [((Col, Row), [Viable])] -> Size -> IO [((Col, Row), [Viable])]
  runner byLen dimens = do
    checkData <- checking 0 0 byLen dimens []
    let chDataList = chunkUp dimens (listifyGrid (sort checkData) 0 [])
    let byCol = smush chDataList 0 []
    --putStrLn ("bc: "++(show byCol))
    let byRow = rearrange byCol 0 []
    --putStrLn ("br: "++ (show byRow)) 
    if (noInnerDupes byCol) && (noInnerDupes byRow)
      then return (checkData)
      else do  (runner byLen dimens)

  noInnerDupes :: [[Viable]] -> Bool
  noInnerDupes [] = True
  noInnerDupes list
    | (length chker) /= (length this) = False
    | otherwise = noInnerDupes shorter
    where this = head list
          chker = nub this
          shorter = tail list
           
  smush :: [[[Viable]]] -> Idx -> [[Viable]] -> [[Viable]]
  smush grid idx accum
    | idx < (length grid) = smush grid (idx + 1) (accum ++ [tmpCon])
    | otherwise = accum
    where tmp  = grid !! idx
          tmpCon = concat tmp

  rearrange :: [[Viable]] -> Idx -> [[Viable]] -> [[Viable]]
  rearrange [] idx accum = accum
  rearrange rows idx accum
    | idx < (length rows) = rearrange rows (idx+1) (accum ++ [taken])
    | idx == (length rows) = accum ++ [(map (last) rows)]
    | otherwise = accum
    where taken = (map (!! idx) rows)

  checking :: Idx -> Idx  -> [((Col, Row), [Viable])] -> Size -> [((Col, Row), [Viable])] ->  IO [((Col, Row), [Viable])]
  checking idx extra gridData dimens builder = do
    let xidx = if (idx == (length gridData)) then 0 else idx
    let justVals = snd (unzip gridData)
    if (allSameLength justVals)
      then return builder
      else do
        let justCoords = fst (unzip gridData)
            thisCoord = (justCoords !! xidx)
            shortList = justVals !! xidx
            newExtra = if (extra > (li shortList)) then 0 else extra 
            adj = getAdjacent gridData thisCoord (dimens)
        picked<-pick shortList
        let sel =  if ((length shortList) > 1) then picked else head shortList
        let subAdj = (removeFromAdjacent adj sel)
            updl = (updateNew gridData subAdj [])
            spud = splitAt xidx updl
            thisShorter = [(thisCoord, [sel])]
            shorterUpdl = (fst spud) ++ thisShorter ++ (tail(snd spud))
            chunkedUpdl = chunkUp dimens (listifyGrid (sort shorterUpdl) 0 [])
            byCol = smush chunkedUpdl 0 []
            byRow = rearrange byCol 0 []
            sortUpdl = sortBy (comparing (length . snd)) shorterUpdl
            newIdx = indexOfPair thisCoord sortUpdl 0
        if (noInnerDupes byCol) || (noInnerDupes byRow)
          then do checking (newIdx) (extra+1) sortUpdl dimens sortUpdl
          else do checking (newIdx + 1) (extra+1) sortUpdl dimens sortUpdl

  walkPaths :: Idx -> Idx -> [((Col, Row), [Viable])] -> Size -> [((Col, Row), [Viable])] ->  IO [((Col, Row), [Viable])]
  walkPaths innerIdx idx gridData dimens builder = do
    let xidx = if (idx == (length gridData)) then 0 else idx
        justCoords = fst (unzip builder)
        justVals = snd (unzip builder)
        theseVals = justVals !! xidx
    if (not (allSameLength justVals)) 
      then do
        let nInnerIdx = if (innerIdx == (li theseVals)) then 0 else innerIdx
            outpicked = if ((length theseVals) == 1) then (head theseVals) else (theseVals !! nInnerIdx)
            thisCoord = justCoords !! nInnerIdx
            adj = getAdjacent gridData thisCoord dimens
            updatedAdj = removeFromAdjacent adj outpicked
            updatedList = updateNew gridData updatedAdj []
        walkPaths (nInnerIdx+1) (idx+1) gridData dimens updatedList 
      else do 
        let chunkedUpdl = chunkUp dimens (listifyGrid (sort gridData) 0 [])
            byCol = smush chunkedUpdl 0 []
            byRow = rearrange byCol 0 []
        if ((noInnerDupes byCol) && (noInnerDupes byRow)) 
          then return (builder)
          else do 
            let backCoords = fst (unzip gridData)
                backVals = snd (unzip gridData)
                theseBackVals = backVals !! (xidx-1)
                thisBackCoord = backCoords !! (xidx-1)
                picked = head theseBackVals
                adj = getAdjacent gridData thisBackCoord dimens
                updatedAdj = removeFromAdjacent adj picked
                updatedList = updateNew gridData updatedAdj []
            walkPaths innerIdx xidx gridData dimens updatedList

  removeFromAdjacent :: [((Col, Row), [Viable])] -> Target -> [((Col, Row), [Viable])]
  removeFromAdjacent dat target = lUpd ++ shortGuys
      where longerVals = filter ((> 1) . length . snd) dat -- ensure list with 1 item does not have val removed
            shortGuys = filter ((== 1) . length . snd) dat
            lCoord = fst (unzip longerVals)
            lVal = snd (unzip longerVals)
            filt = map (filter (/= target)) lVal
            lUpd = zip lCoord filt
            
  updateNew :: [((Col, Row), [Viable])] -> [((Col, Row), [Viable])] -> [((Col, Row), [Viable])] -> [((Col, Row), [Viable])]
  updateNew _ [] final = final
  updateNew initList updated final = updateNew new (tail updated) new
    where
      fChunk = (head updated)
      fElem = fst fChunk
      pidx = indexOfPair fElem initList 0
      spl = splitAt (pidx) initList
      new = (fst spl) ++ [fChunk] ++ (tail (snd spl))

  according :: [((Col, Row), [Viable])] -> (Col, Row) -> ((Col, Row), [Viable])
  according dat pair = dat !! idx
    where idx = indexOfPair pair dat 0

  pick :: [Int] -> IO Int
  pick list = do
    rand <- randomRIO (0, (li list))
    if (length list) == 1 
      then return (head list)
      else do
        return (list !! rand)


  allSameLength :: [[a]] -> Bool
  allSameLength []     = True
  allSameLength (noggin : body) = all (\inner -> length inner == length noggin) body

  getPossible :: (Col, Row) -> [((Col, Row), [Viable])] -> [Viable]
  getPossible (c, r) dList = snd (dList !! idx)
    where idx = indexOfPair (c, r) dList 0

  shortest :: [[Int]] -> [Int]
  shortest [] = []
  shortest [noggin] = noggin
  shortest (noggin : body) = let tiniest = shortest body
                             in if (length tiniest) < (length noggin) 
                                then tiniest else noggin

  equalPairs :: (Col, Row) -> (Col, Row) -> Bool
  equalPairs (fstCol, fstRow) (sndCol, sndRow) = ((fstCol == sndCol) && (fstRow == sndRow))


  getAdjacent ::  [((Col, Row),[Viable])] -> ((Col, Row))->Size -> [((Col, Row),[Viable])]
  getAdjacent dat (c, r)  d
    | equalPairs (c,r) (0,0) = [below, right]
    | equalPairs (c,r) (0,(d - 1)) = [above, right]
    | equalPairs (c,r) ((d - 1),0) = [below, left]
    | equalPairs (c,r) ((d - 1),(d - 1)) = [above, left]
    | (c == (d - 1)) = [above, below, left]
    | (c == 0) = [above, below, right]
    | (r == (d - 1))  = [above, left, right]
    | (r == 0) = [below, left, right]
    | otherwise = [above, below, left, right]
    where right = according dat ((c+1), r)
          left = according dat ((c-1), r)
          below = according dat (c, (r+1))
          above = according dat (c, (r-1))

  process :: [Flne] -> Idx -> Count -> Size -> [(Mark, Viable)] -> IO [(Mark, Viable)]
  process line idx count size coords = do
    if count == 0
      then return coords
      else do
        let thisLn = (line !! idx)
            potenVals = findVals thisLn size
            target = getCageTarget thisLn
        potPairs <- possiblePairs (words thisLn) potenVals target
        let pairs = sort (nub (potPairs))
        putStrLn ( "cage: " ++ (show (head thisLn)) ++ " op: " ++ (show (last thisLn)) ++
                   " val: " ++ (show target) ++ " combos: " ++ (show potenVals))
        process line (idx + 1) (count - 1) size (coords ++ pairs)

  mkList :: [((Col, Row), [Viable])] -> [((Col, Row), [Viable])] -> [((Col, Row), [Viable])]
  mkList [] accum = accum
  mkList items accum
    | (length vals) == 1 = mkList (tail items) (accum ++ [(head items)])
    | otherwise = mkList (tail items) accum
    where vals = snd (head items)

  chooseIndex :: [Viable] -> Int -> Idx
  chooseIndex gdlist num
    | (li gdlist) < (num) = (num `rem` 3)
    | otherwise = num

  possiblePairs :: [Mark] -> [[Viable]] -> Target -> IO [(Mark, Viable)]
  possiblePairs str vals target = do
    let size = (read (head str)) :: Int
    if size == 1
      then return [((str !! 1), target)]
      else do
        coordPairs <- outerPairer (permutations (init (init (tail str)))) vals [] 0
        return (coordPairs)

  outerPairer :: [[Mark]] -> [[Viable]] -> [(Mark, Viable)] -> Idx -> IO [(Mark, Viable)]
  outerPairer str vals accum idx = do
    if (length vals) == idx
      then return accum
      else do
        pairOne <- innerPairer str (vals !! idx) [] 0
        outerPairer str vals (accum ++ pairOne) (idx + 1)

  innerPairer :: [[Mark]] -> [Viable] -> [(Mark, Int)] -> Idx -> IO [(Mark, Int)]
  innerPairer str vals accum idx = do
    if (length str) == idx
      then return (accum)
      else do innerPairer str vals (accum ++ (zip (str !! idx) vals)) (idx + 1)

  doSub :: String -> Size -> [[Int]]
  doSub chars maxNum
    | cage > 3 = subHelper (getCageTarget chars) (findSubsets maxNum (cage-1)) []
    | otherwise = subHelper (getCageTarget chars) (findSubsets maxNum cage) []
    where cage = (digitToInt (head (filter isDigit chars)))

  subHelper :: Target -> [[Int]] -> [[Viable]] -> [[Viable]]
  subHelper target [] accum = accum
  subHelper target sets accum = subHelper target (dropAt sets 0) updated
    where updated = accum ++ (subFold target ([(head sets)]) [])

  subFold :: Target -> [[Int]] -> [[Viable]] -> [[Viable]]
  subFold target [] valid = valid
  subFold target perms valid
    | total == target = subFold target lessPerms (valid ++ [this])
    | otherwise = subFold target lessPerms valid
    where this = head perms
          lessPerms = filter (/= this) perms
          total = abs (foldl (-) (head this) (tail this))

  doAdd :: String -> Size -> [[Viable]]
  doAdd chars maxNum
    | cage > 3 = addHelper (findSubsets maxNum (cage)) [] (getCageTarget chars) 0
    | otherwise = addHelper (findSubsets maxNum cage) [] (getCageTarget chars) 0
    where cage = (digitToInt (head (filter isDigit chars)))

  addHelper :: [[Int]] -> [[Viable]] -> Target -> Idx -> [[Viable]]
  addHelper [] accum target idx = accum
  addHelper list accum target idx
    | (li list) == idx = accum
    | ((sum (list !! idx)) == target) && ((li list) /= idx) = addHelper list (accum ++ [(list !! idx)]) target (idx + 1)
    | ((sum (list !! idx)) /= target) && ((li list) /= idx) = addHelper list accum target (idx + 1)

  findSubsets :: Size -> Size -> [[Viable]]
  findSubsets maxNum cageSize = sort (uniqLists (setGenerator cageSize legal) 0)
    where legal = replConcat (getLegalVals maxNum) cageSize

  indexOf :: Eq a => a -> [a] -> Idx -> Idx
  indexOf item arr idx
    | item == (arr !! idx) = idx
    | item /= (arr !! idx) = indexOf item arr (idx + 1)
    | otherwise = -1

  indexOfPair :: Eq a => (a, a) -> [((a, a), [a])] -> Idx -> Idx
  indexOfPair item arr idx 
    | item == (fst (arr !! idx)) = idx
    | item /= (fst (arr !! idx)) = indexOfPair item arr (idx + 1)
    | otherwise = -1

  getCageTarget :: Flne -> Target
  getCageTarget str
    | cageSze == 1 = noOper
    | str == "" = 0
    | otherwise = target
    where wList = words str
          noOper = read (last wList) :: Target
          cageSze = read (head wList) :: Target 
          target = read (last (init wList)) :: Target

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

  setGenerator :: Size -> [Int] -> [[Int]]
  setGenerator 0 _ = [[]]
  setGenerator _ [] = []
  setGenerator lim (noggin : body) = [noggin : subs | subs <- setGenerator (lim - 1) body] ++ setGenerator lim body

  replConcat :: [Int] -> Count -> [Int]
  replConcat list times
    | times > 3 = sort (concat (replicate (times-2) list))
    | otherwise = sort (concat (replicate times list))

  uniqLists :: Ord a => Eq a => [[a]] -> Idx -> [[a]]
  uniqLists [] idx = []
  uniqLists list idx
    | idx < (li list) = uniqLists ([tmpList] ++ (filter (/= tmpList) list)) (idx + 1)
    | otherwise = list
    where tmpList = (list !! idx)

  groupValues :: [(Mark, Viable)] -> [(Mark, [Viable])]
  groupValues = map (\list -> (fst . head $ list, map snd list)) . groupBy ((==) `on` fst) . sortBy (comparing fst)

  chunkUp :: Idx -> [a] -> [[a]]
  chunkUp _ [] = []
  chunkUp count ns =
    let (os, ps) = splitAt count ns
      in os : chunkUp count ps

  dropAt :: [a] -> Idx -> [a]
  dropAt list idx = take idx list ++ drop (idx + 1) list

  li :: [a] -> Int
  li str = (length str) - 1

  form :: (Read a, Read b, Show a, Show b) => Len -> Count -> [(a, b)] -> String -> String
  form 0 flag pairs accum = accum
  form len flag pairs accum = formed
    where coord = show (fst (head pairs))
          num = show (snd (head pairs))
          out = accum ++ "" ++ coord ++ " --> " ++ num ++ " " ++ (fst (endsep flag))
          cut = drop 1 pairs
          formed = form (length cut) (snd (endsep flag)) cut out

  endsep :: Count -> (String, Int)
  endsep 1   = ("\n", brFlag)
  endsep cnt = ("\t", (cnt - 1))

  formGrid :: [[[Viable]]] -> Idx -> String -> String
  formGrid grid idx accum
    | ((li grid) + 1) == idx = accum ++ "\n"
    | otherwise = formGrid grid (idx + 1) (accum ++ (show (grid !! idx)) ++ "\n")

  listifyGrid :: [(a, [Viable])] -> Idx -> [[Viable]] -> [[Viable]]
  listifyGrid coordVals idx accum
    | (li coordVals) == idx = accum ++ vals
    | otherwise = listifyGrid coordVals (idx + 1) (accum ++ vals)
    where
      vals = [snd (coordVals !! idx)]

  findVals :: Flne -> Size -> [[Int]]
  findVals list size
    | (last list) == '-' = doSub list size
    | (last list) == '+' = doAdd list size
    | otherwise = [[(digitToInt (last list))]]

  uniqGrid :: Ord a => Eq a => [((a, a), [a])] -> Idx -> [((a, a), [a])]
  uniqGrid list idx
    | idx < (li list) =  uniqGrid ([((fst tmp), (snd tmp))] ++ shortened) (idx + 1)
    | otherwise = list
    where tmp = list !! idx
          shortened = filter (/= tmp) list

  colRowPair :: (Mark, [Viable]) -> ((Col, Row), [Viable])
  colRowPair coords = ((numCol, irow), (snd coords))
    where irow = ((read [(fst coords) !! 1]) - 1)
          numCol = ((indexOf (head (fst coords)) colNames 0))

  rowColPair :: (Mark, [Viable]) -> ((Row, Col), [Viable])
  rowColPair coords = ((irow, numCol), (snd coords))
    where irow = ((read [(fst coords) !! 1]) - 1)
          numCol = ((indexOf (head (fst coords)) colNames 0))

  type Count = Int
  type Col = Int
  type Idx = Int
  type Len = Int
  type Row = Int
  type Size = Int
  type Target = Int
  type Viable = Int

  type Flne = String
  type Mark = String
  