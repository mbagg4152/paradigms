{-# OPTIONS_GHC -Wall
    -fprof-auto -fprof-cafs
    -fbreak-on-exception
    -rtsopts
     #-}
{-# LANGUAGE ExplicitForAll #-}

  module Main where
  import System.IO
  import Data.Char
  import Data.Function
  import Data.List
  import Data.Ord
  import System.Environment
  --import System.Random
  import Debug.Trace
  --import Data.Tree
  import Data.Maybe
  brFlag :: Int
  brFlag = 4

  colNames :: [Char]
  colNames = "abcdef"

  dtag :: [Char]
  dtag = "db.db"

  dbpath :: [Char]
  dbpath = "debug.log"
  
  data Cell = F Int | P [Int] deriving (Show, Eq)
  type Row  = [Cell]
  type Grid = [Row]

  



  main :: IO ()
  main = do
    writeFile dbpath ""
    fName <- getArgs
    let fNoRet = filter (/= '\r') (fName !! 0)
    fcontent <- readFile fNoRet
    let fLines = lines fcontent
        fLineNoret = map (filter (/= '\r')) fLines
        dimens = read (fLineNoret !! 0) :: Int
    ps ("Board size: " ++ (s dimens))

    coords <- process (tail fLineNoret) 0 (length (tail fLineNoret)) dimens []
    let groups = groupValues coords
        revGrp = sort (map rowColPair groups)
        normGrp = sort (map colRowPair groups)
        normGStr = form (length normGrp) brFlag normGrp ""
    ps ("\n\nmade using coords (col,row):\n" ++ normGStr++ "\n")

    let byLen = sortBy (comparing (length . snd)) normGrp
        byMk = sortBy (comparing fst) normGrp
        lenList = chunkUp dimens (listifyGrid byLen 0 [])
        lenStr = formGrid lenList 0 ""
    
    -- walked <- walkPaths 0 0 byLen dimens byLen 
    -- let wDataList = chunkUp dimens (listifyGrid (sort walked) 0 [])
    --     wDataStr = formGrid wDataList 0 ""

    let emg = mkEmpty dimens dimens
        llen = idxList byLen 0 []
        iGrid = initGrid 0 revGrp []
        fGrid = chunkUp dimens iGrid
        gridStr = dispGrid fGrid
        fullGridStr = dispFullGrid fGrid dimens
        

    ps gridStr
    --ps fullGridStr
    let pruned = fromJust (traverse pruneCells fGrid)
        prunedAgain = fromJust (traverse pruneCells pruned)

    let shifted = transpose fGrid
        colPruned = fromJust (traverse pruneCells shifted)
    ps $ dispFullGrid pruned dimens
    ps $ dispFullGrid colPruned dimens
    ps $ s $ length pruned


  initGrid :: Idx -> [((Int, Int), [Int])] -> Row -> Row
  initGrid  idx dat accum
    | (idx < gLen) && (numLen == 1) = initGrid (idx + 1) dat (accum ++ [fxInt])
    | (idx < gLen) && (numLen > 1) = initGrid  (idx + 1) dat (accum ++ [pNums])
    | otherwise = accum
    where thisDat = dat !! idx
          row = snd $ fst thisDat
          col = fst $ fst thisDat
          nums = snd thisDat
          pNums = P (nums)
          numLen = length nums
          gLen = length dat
          fxInt = F (head nums)

          
          --updated = updateGrid grid (head vals) (row, col)
  	
  dispFullGrid :: Grid -> Size -> String
  dispFullGrid grid size =  unlines (map (unwords . map showCell) grid)
    where
      showCell (F x)
        | size == 4 = "[" ++ show x ++ "   ]"
        | size == 5 = "[" ++ show x ++ "    ]"
        | size == 6 = "[" ++ show x ++ "     ]"
      showCell (P xs) = (++ "]") . foldl' (\acc x -> acc ++(if x `elem` xs then show x else " ")) "[" $ [1..size]
  

  ns :: [((Int, Int), [Int])] -> Idx -> Size -> [((Int, Int), [Int])] -> [((Int, Int), [Int])]
  ns dat idx size accum
    | keepGoing && (numLen == 1) =
    | keepGoing && (numLen > 1) =
    | otherwise = accum
    where keepGoing = idx < (length dat)
          thisDat = dat !! idx 
          thisMk = fst thisDat
          theseNums = snd thisDat
          numLen = length theseNums



  	
  pruneCells :: [Cell] -> Maybe [Cell]
  pruneCells cells = traverse pruneCell cells
    where
      fixeds = [x | F x <- cells]
      pruneCell (P xs) = case xs Data.List.\\ fixeds of
        []  -> Nothing
        [y] -> Just $ F y
        ys  -> Just $ P ys
      pruneCell x = Just x

  dispGrid :: Grid -> String
  dispGrid = unlines . map (unwords . map showCell)
    where showCell (F x) = show x
          showCell _ = "-"

  {-data Tree a = L a | N a (Tree a) (Tree a) | Z deriving (Show)
    lsTree [] = Z
    lsTree [x] = L x
    lsTree list = N x (lsTree ltx) (lsTree gtx)
                 where 
                 m = length list `div` 2
                 x = list !! m
                 ltx = take m list
                 gtx = drop (m+1) list
   -}
  {-finder :: Size -> Step -> Count -> Count -> 
              [((Sc, Sr), [Viable])] -> [[((Sc, Sr), [Viable])]] -> 
              [[Viable]] -> [[[Viable]]] -> [[Idx]] -> [[[Idx]]]  ->
              IO [((Sc, Sr), [Viable])]
    finder bd steps mkPos idxStep inp inpBin grid gridBin idxList idxBin = do
      ps "-------------------------------------------------------------------------"
      traceShowM (length gridBin)
      traceShowM (length inpBin)
      traceShowM steps
      --traceShowM idxList
      --traceShowM idxBin
      let marksOnly = fst (unzip inp)
          numsOnly = snd (unzip inp)

      if (mkPos == (length inp))
        then do 
          ps ">>>>  1. completed one full list iteration  <<<<"
          if (allSameLength numsOnly)
            then do
              ps ">>>>  1a. final grid is filled  <<<<"
              return inp
            else do 
              ps ">>>>  1b. some paths still need to be taken  <<<<"
              finder bd steps 0 idxStep inp inpBin grid gridBin idxList idxBin
        else do
          ps (">>>>  2. look at current point & select val/path  <<<<")
          traceShowM (snd (unzip (sort inp)))        
          let thisMark = marksOnly !! mkPos
              thisCol = fst thisMark
              thisRow = snd thisMark
              theseIdxs = idxList !! mkPos
              thisIdx = head theseIdxs
          if ((length theseIdxs) == 1) 
            then do
              ps (">>>>  3. at last value for the current point, determine what to do  <<<<")
              traceShowM (length numsOnly)
              traceShowM (mkPos)
              let picked = last (numsOnly !! mkPos)
              let focusedRow = grid !! thisRow   
                  focusedCol = (rearrange grid 0 []) !! thisCol
              if ((picked `elem` focusedRow) || (picked `elem` focusedCol))
                then do
                  ps (">>>>  4. the last value doesn't fit in the grid, need to backtrack  <<<<")
                  ps "changes idx - since val is unsuccessful, remove its index"
                  let savePt = if (steps > 0) then (steps - 1) else 0
                  let oldInp = inpBin!!savePt
                      oldGrid = gridBin!!savePt

                      idxLen = length idxBin
                      diff = abs (idxLen - idxStep) 
                      oldIdxs = idxBin!!savePt
                      

                  
                  let rmIdx = if ((length theseIdxs) > 1) then filter (/= thisIdx) theseIdxs else [head theseIdxs]
                      splIdx = splitAt mkPos idxList
                      newIdxs = (fst splIdx) ++ [rmIdx] ++ (tail (snd splIdx))
                  let cleanInpBin = dropAt inpBin savePt
                      cleanGridBin =   dropAt gridBin savePt
                      cleanIdxBin =  dropAt idxBin savePt
                      
                  let prevMkCount = if (mkPos > 0) then (mkPos - 1) else 0
                  finder bd (steps - 1) prevMkCount 0 oldInp cleanInpBin oldGrid cleanGridBin newIdxs cleanIdxBin   

                else do
                  ps (">>>>  5. last value of current point fits in grid, look at next point  <<<<")
                  ps "changes idx - since val is successful, leave it as only index in place"
                  let newGrid = updateGrid grid picked (thisRow, thisCol)
                      nGridBin = gridBin ++ [grid]
                  let adj = getAdjacent inp (thisCol, thisRow) bd
                      lessAdj = removeFromAdjacent adj picked
                      newInp = updateNew inp lessAdj []
                      nInpBin = inpBin ++ [inp]
                      tIdx = mkPos
                      splIdx = splitAt tIdx idxList
                      newIdxs = (fst splIdx) ++ [[thisIdx]] ++ (tail (snd splIdx))
                      nIdxBin = idxBin ++ [idxList]
                  finder bd (steps + 1) (mkPos + 1) (idxStep + 1) newInp nInpBin newGrid nGridBin newIdxs nIdxBin

            else do
              ps (">>>>  6. not at the end of the value list, keep going  <<<<")
              ps (s grid)
              let ivCount = head theseIdxs
              traceShowM (length numsOnly)
              traceShowM ivCount
              traceShowM mkPos
              traceShowM (length (numsOnly !! mkPos))

              let picked = if (length (numsOnly !! mkPos)) == ivCount then last (numsOnly !! mkPos) else (numsOnly !! mkPos) !! ivCount
              let focusedRow = grid !! thisRow   
              let focusedCol = (rearrange grid 0 []) !! thisCol 
              if ((picked `elem` focusedRow) || (picked `elem` focusedCol))
                then do
                  ps (">>>>  7. value is taken up in row and/or column, move to next  <<<<")
                  ps "changes idx - since val is successful, leave it as only index in place"
                  ps (s grid)
                  ps (s picked)
                  ps (s focusedRow)
                  ps (s focusedCol)
                  let fIdxs = if ((length theseIdxs) == 1) then [(head theseIdxs)] else (tail theseIdxs)
                      splIdx = splitAt mkPos idxList
                      newIdxs = (fst splIdx) ++ [fIdxs] ++ (tail (snd splIdx))
                      nIdxBin = idxBin ++ [newIdxs] 
                  finder bd steps mkPos (idxStep + 1) inp inpBin grid gridBin newIdxs nIdxBin

                else do
                  ps (">>>>  8. value is NOT taken up in row or column, make updates  <<<<")
                  ps "changes idx - since val is unsuccessful, remove its index"
                  let newGrid = updateGrid grid picked (thisRow, thisCol)
                      nGridBin = gridBin ++ [grid]
                  let adj = getAdjacent inp (thisCol, thisRow) bd
                      lessAdj = removeFromAdjacent adj picked
                      newInp = updateNew inp lessAdj []
                      nInpBin = inpBin ++ [inp]
                      tIdx = mkPos
                      splIdx = splitAt tIdx idxList
                      newIdxs = (fst splIdx) ++ [[thisIdx]] ++ (tail (snd splIdx))
                      nIdxBin = idxBin ++ [idxList]
                  traceShowM picked    
                  finder bd (steps + 1) (mkPos + 1) (idxStep + 1) newInp nInpBin newGrid nGridBin newIdxs nIdxBin 
                  -}
  
  idxList :: [((Sc, Sr), [Viable])] -> Idx -> [[Idx]] -> [[Idx]]
  idxList list idx accum
    | idx < (length list) = idxList list (idx+1) (accum ++ [lr])
    | otherwise = accum
    where this = list !! idx
          nums = snd this
          len = length nums
          lr = [0..(len-1)]
     
  updateGrid :: [[a]] -> a -> (Int, Int) -> [[a]]
  updateGrid m x (r,c) =
    take r m ++
    [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
    drop (r + 1) m

  getAdjacent ::  [((Sc, Sr),[Viable])] -> (Sc, Sr) -> Size -> [((Sc, Sr),[Viable])]
  getAdjacent dat (c, r)  d
    | equalPairs (c,r) (0,0) = [below, right]
    | equalPairs (c,r) (0,(d - 1)) = [above, right]
    | equalPairs (c,r) ((d - 1),0) = [below, left]
    | equalPairs (c,r) ((d - 1),(d - 1)) = [above, left]
    | ((c == (d-1)) && noExtRow)  = [above, below, left]
    | ((c == 0) && noExtRow) = [above, below, right]
    | ((r == (d - 1)) && noExtCol)  = [above, left, right]
    | ((r == 0) && noExtCol) = [below, left, right]
    | otherwise = [above, below, left, right]
    where right = according dat ((c+1), r)
          left = according dat ((c-1), r)
          below = according dat (c, (r+1))
          above = according dat (c, (r-1))
          noExtRow = ((r /= 0) && (r /= (d - 1)))
          noExtCol = ((c /= 0) && (c /= (d - 1)))

  removeFromAdjacent :: [((Sc, Sr), [Viable])] -> Target -> [((Sc, Sr), [Viable])]
  removeFromAdjacent dat target = lUpd ++ shortGuys
      where longerVals = filter ((> 1) . length . snd) dat -- ensure list with 1 item does not have val removed
            shortGuys = filter ((== 1) . length . snd) dat
            lCoord = fst (unzip longerVals)
            lVal = snd (unzip longerVals)
            filt = map (filter (/= target)) lVal
            lUpd = zip lCoord filt

  updateNew :: [((Sc, Sr), [Viable])] -> [((Sc, Sr), [Viable])] -> [((Sc, Sr), [Viable])] -> [((Sc, Sr), [Viable])]
  updateNew _ [] final = final
  updateNew initList updated final = updateNew new (tail updated) new
    where
      fChunk = (head updated)
      fElem = fst fChunk
      pidx = indexOfPair fElem initList 0
      spl = splitAt (pidx) initList
      new = (fst spl) ++ [fChunk] ++ (tail (snd spl))

  runner :: [((Sc, Sr), [Viable])] -> Size -> IO [((Sc, Sr), [Viable])]
  runner byLen dimens = do
    checkData <- checking 0 0 byLen dimens []
    let chDataList = chunkUp dimens (listifyGrid (sort checkData) 0 [])
    let byCol = smush chDataList 0 []
    let byRow = rearrange byCol 0 [] 
    if (noInnerDupes byCol) && (noInnerDupes byRow)
      then return (checkData)
      else do  (runner byLen dimens)

  mkEmpty :: Size -> Count -> [[Int]]
  mkEmpty n 0 = []
  mkEmpty n count = [(take n [0,0..])] ++ mkEmpty n (count - 1)

  bigLineTime :: Show a => [[a]] -> String 
  bigLineTime line = concat (map lineTime line)

  lineTime :: Show a => [a] -> String
  lineTime line = ((s line) ++ "\n\n")

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

  checking :: Idx -> Idx  -> [((Sc, Sr), [Viable])] -> Size -> [((Sc, Sr), [Viable])] ->  IO [((Sc, Sr), [Viable])]
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
        --picked<-pick shortList
            picked = head shortList
        let sel =  if ((length shortList) > 1) then picked else head shortList
        let subAdj = (removeFromAdjacent adj sel)
            updl = (updateNew gridData subAdj [])
            spud = splitAt xidx updl
            thisShorter = [(thisCoord, [sel])]
            shorterUpdl = (fst spud) ++ thisShorter ++ (tail(snd spud))

            -- test: remove sort
            chunkedUpdl = chunkUp dimens (listifyGrid (shorterUpdl) 0 [])
            byCol = smush chunkedUpdl 0 []
            byRow = rearrange byCol 0 []
            sortUpdl = sortBy (comparing (length . snd)) shorterUpdl
        --debugM dtag ("[checking]"++"before indexOfPairCall: " ++ (s thisCoord) ++ " " ++ (s sortUpdl)) 
        let newIdx = indexOfPair thisCoord sortUpdl 0
        if (noInnerDupes byCol) || (noInnerDupes byRow)
          then do checking (newIdx) (extra+1) sortUpdl dimens sortUpdl
          else do checking (newIdx + 1) (extra+1) sortUpdl dimens sortUpdl

  walkPaths :: Idx -> Idx -> [((Sc, Sr), [Viable])] -> Size -> [((Sc, Sr), [Viable])] ->  IO [((Sc, Sr), [Viable])]
  walkPaths full inner vgrid dimens builder = do
    ps "-------------------------------------------------------------------------"
    let fullLen = length vgrid
        xfull = if ((full == fullLen) || (full < 0)) then 0 else full
        initjustNums = snd (unzip vgrid)
        initchunked = chunkUp dimens (listifyGrid vgrid 0 [])
        initbyCol = smush initchunked 0 []
        initbyRow = rearrange initbyCol 0 []
    if (allSameLength initjustNums) 
      then return vgrid
      else do
        if ((xfull) == fullLen)
          then do 
            ps ">>>> 1. end of input data <<<<"
            ps $ s xfull
            ps $ s fullLen 
            walkPaths 0 (inner + 1) vgrid dimens builder
          else do 
              ps ">>>> 2. not end of input data <<<<"
              let nfull = (xfull + 1)  
                  justMarks = fst (unzip vgrid)
                  justNums = snd (unzip vgrid)
              let theseNums = justNums !! xfull
              ps $ s theseNums


              let thisMark = justMarks !! xfull
              ps $ s thisMark

              let numLen = length theseNums
              ps $ s numLen

              let xinner = if ((numLen == 1) || (inner == numLen)) then 0 else inner
              ps $ s xinner

              let thisNum = theseNums !! xinner
              ps $ s thisNum

              let adj  =  getAdjacent vgrid thisMark dimens
              ps $ s adj

              let updatedAdj = removeFromAdjacent adj thisNum
              let updat = updateNew vgrid updatedAdj []
              let sidx = indexOfPair thisMark vgrid 0
                  spUpdat = splitAt sidx updat
              let thisShort = [(thisMark, [thisNum])]
                  shortUpdat = (fst spUpdat) ++ thisShort ++ (tail (snd spUpdat))
                  shorterJustNums = (snd (unzip shortUpdat))
              let chunked = chunkUp dimens (listifyGrid shortUpdat 0 [])
                  byCol = smush chunked 0 []
                  byRow = rearrange byCol 0 []

              ps $ s byCol
              ps $ s byRow

              if (allSameLength shorterJustNums)
                then do
                  ps ">>>> 3. value choices all narrowed down <<<<"     
                  if ((noInnerDupes byCol) && (noInnerDupes byRow))
                    then do 
                      ps ">>>> 4. all unique values <<<<" 
                      return shortUpdat
                    else do 
                      ps ">>>> 5. at least one duplicate value <<<<"
                      walkPaths (xfull - 1) xinner shortUpdat dimens shortUpdat
                else do 
                  ps ">>>> 6. still need to narrow down the list of values <<<<"
                  if ((numLen == 1) || (inner == numLen))
                    then do 
                      ps ">>>> 7. reached end of inner content <<<<"
                      walkPaths (xfull + 1) 0 shortUpdat dimens shortUpdat
                    else do 
                      ps ">>>> 8. did not reach end of inner content <<<<"
                      walkPaths (xfull) (xinner+1) shortUpdat dimens shortUpdat

  blah :: [Char] -> [Char]
  blah line = (iinsert 111 ' ' (iinsert 110 '\n' line))
  
  ss :: (Ord a , Show a) => [a] -> [Char]
  ss list = s (sort list)

  s :: (Show a) => a -> [Char]
  s item = show item

  ps :: String -> IO ()
  ps str = putStrLn str

  iinsert :: Int -> a -> [a] -> [a]
  iinsert n y xs = countdown n xs where
    countdown 0 xs = y:countdown n xs -- reset to original n
    countdown _ [] = []
    countdown m (x:xs) = x:countdown (m-1) xs


  according :: [((Sc, Sr), [Viable])] -> (Sc, Sr) -> ((Sc, Sr), [Viable])
  according dat pair = dat !! idx
    where idx = indexOfPair pair dat 0

  {-pick :: [Int] -> IO Int
  pick list = do
    rand <- randomRIO (0, (li list))
    if (length list) == 1 
      then return (head list)
      else do
        return (list !! rand)-}


  allSameLength :: [[a]] -> Bool
  allSameLength []     = True
  allSameLength (noggin : body) = all (\inner -> length inner == length noggin) body

  getPossible :: (Sc, Sr) -> [((Sc, Sr), [Viable])] -> [Viable]
  getPossible (c, r) dList = snd (dList !! idx)
    where idx = indexOfPair (c, r) dList 0

  shortest :: [[Int]] -> [Int]
  shortest [] = []
  shortest [noggin] = noggin
  shortest (noggin : body) = let tiniest = shortest body
                             in if (length tiniest) < (length noggin) 
                                then tiniest else noggin

  equalPairs :: (Sc, Sr) -> (Sc, Sr) -> Bool
  equalPairs (fstCol, fstRow) (sndCol, sndRow) = ((fstCol == sndCol) && (fstRow == sndRow))

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
        ps ( "cage: " ++ (s (head thisLn)) ++ " op: " ++ (s (last thisLn)) ++
                   " val: " ++ (s target) ++ " combos: " ++ (s potenVals))
        process line (idx + 1) (count - 1) size (coords ++ pairs)

  mkList :: [((Sc, Sr), [Viable])] -> [((Sc, Sr), [Viable])] -> [((Sc, Sr), [Viable])]
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
    | ((li arr) == idx) && (item == thisPair) = idx
    | ((li arr) == idx) && (item /= thisPair) =  -1
    | item == thisPair = idx
    | item /= thisPair = indexOfPair item arr (idx + 1)
    | otherwise =   -1
    where thisPair = (fst (arr !! idx)) 
           
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

  chunkUp :: Show a => Idx -> [a] -> [[a]]
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
    where coord = s (fst (head pairs))
          num = s (snd (head pairs))
          out = accum ++ "" ++ coord ++ " --> " ++ num ++ " " ++ (fst (endsep flag))
          cut = drop 1 pairs
          formed = form (length cut) (snd (endsep flag)) cut out

  endsep :: Count -> (String, Int)
  endsep 1   = ("\n", brFlag)
  endsep cnt = ("\t", (cnt - 1))

  formGrid :: [[[Viable]]] -> Idx -> String -> String
  formGrid grid idx accum
    | ((li grid) + 1) == idx = accum ++ "\n"
    | otherwise = formGrid grid (idx + 1) (accum ++ (s (grid !! idx)) ++ "\n")

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

  colRowPair :: (Mark, [Viable]) -> ((Sc, Sr), [Viable])
  colRowPair coords = ((numCol, irow), (snd coords))
    where irow = ((read [(fst coords) !! 1]) - 1)
          numCol = ((indexOf (head (fst coords)) colNames 0))

  rowColPair :: (Mark, [Viable]) -> ((Sr, Sc), [Viable])
  rowColPair coords = ((irow, numCol), (snd coords))
    where irow = ((read [(fst coords) !! 1]) - 1)
          numCol = ((indexOf (head (fst coords)) colNames 0))

  type Count = Int
  type Sc = Int
  type Idx = Int
  type Len = Int
  type Sr = Int
  type Size = Int
  type Step = Int
  type Target = Int
  type Viable = Int

  type Flne = String
  type Mark = String
  