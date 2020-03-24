{-# LANGUAGE ExplicitForAll #-}
{-# OPTIONS_GHC 
    -fbreak-on-exception
    -fprof-auto 
    -fprof-cafs
    -rtsopts
    -Wincomplete-patterns
    -Wname-shadowing
    
   
#-}

{- 
  maybe need to use later:
  -Wall
  -Wunused-imports
  -Wunused-local-binds
  -Wunused-matches
  -fdiagnostics-color=⟨always|auto|never⟩
   -fdiagnostics-show-caret

-}







  module Main where

  import Data.Char
  import Data.Function
  import Data.List
  import Data.Maybe
  import Data.Ord
  import System.Environment

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
  

  -- ns :: [((Int, Int), [Int])] -> Idx -> Size -> [((Int, Int), [Int])] -> [((Int, Int), [Int])]
  -- ns dat idx size accum
  --   | keepGoing && (numLen == 1) =
  --   | keepGoing && (numLen > 1) =
  --   | otherwise = accum
  --   where keepGoing = idx < (length dat)
  --         thisDat = dat !! idx 
  --         thisMk = fst thisDat
  --         theseNums = snd thisDat
  --         numLen = length theseNums



  	
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




  allSameLength :: [[a]] -> Bool
  allSameLength []     = True
  allSameLength (noggin : body) = all (\inner -> length inner == length noggin) body




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
  