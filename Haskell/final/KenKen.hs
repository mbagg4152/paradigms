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
    -fdefer-type-errors
  -}

  module Main where

  import Data.Char
  import Data.Function
  import Data.List
  import Data.Maybe
  import Data.Ord
  import System.Environment

  import Debug.Trace
  import Control.Monad
  import Data.Foldable 
  import Data.Sequence ((><), fromList)
  import Data.List.Utils




  
  brfl :: N
  brfl = 4

  colNames :: [Char]
  colNames = "abcdef"

  dlbl :: [Char]
  dlbl = "dbug"
  
  data Cell = F N | P [N] deriving (Show, Ord, Eq)
  type Row  = [Cell]
  type Grid = [Row]
  type Pair = (N,N)
  type Chunk = ((N, N), [N])
  type Clump = ((N,N), [[N]])

  
  main :: IO ()
  main = do
    -- updateGlobalLogger dlbl (setLevel DEBUG)
    fName <- getArgs
    let fNoRet = filter (/= '\r') (fName !! 0)
    fcontent <- readFile fNoRet
    let fLines = lines fcontent
        fLineNoret = map (filter (/= '\r')) fLines
        dimens = read (fLineNoret !! 0) :: N
    ps ("Board size: " ++ (s dimens))

    coords <- process (tail fLineNoret) 0 (length (tail fLineNoret)) dimens []
    
    let groupValues = map (\list -> (fst . head $ list, map snd list)) . 
                           groupBy ((==) `on` fst) . sortBy (comparing fst)
        groups = groupValues coords
        revGrp = sort (map rowColPair groups)
        normGrp = sort (map colRowPair groups)
        normGStr = form dimens brfl revGrp ""
    ps ("\n\nmade using coords (row,col):\n" ++ normGStr++ "\n")

    let byLen = sortBy (comparing (length . snd)) normGrp
        emg = mkEmpty dimens dimens

        pr1 = pr revGrp dimens
        swapped = sort (swapper pr1)

        prc = pr normGrp dimens
        pr2 = pr pr1 dimens


    proc2 <- process2 (tail fLineNoret) 0 (length (tail fLineNoret)) dimens []
    let uz = chunkUp dimens (snd (customUnzip (sort proc2)))
        other = transpose uz

        rcclump =  sort (map rowColClump (sort proc2) )
        prcl = prClump rcclump dimens
        prsw = sort(clumpSwap prcl)
        prswpr = prClump prsw dimens
        conden = condense prswpr

        condenlist = listifyGrid conden 0 []

        condenseq = sequence condenlist
    fconden <- fseq condenseq dimens

        
    -- ps $ s  rcclump
    -- ps " "
    -- ps $ s  prcl
    -- ps " "
    -- ps $ s  (sort (clumpSwap prswpr))
    ps $ s condenlist
    
    
    if ((length fconden == 0))
      then do 
        ps "no solutions :("
      else do
        ps $ s fconden
   
    
    ps " "

    ps " "

  condense :: [Clump] -> [Chunk]
  condense clump = loop 0 clump []
    where
      loop idx cl accum
        | (idx < (length cl)) = loop (idx+1) cl (accum++[(mks,cond)])
        | otherwise = accum
          where this = cl !! idx
                mks = fst this
                cond = nub (concat (snd this))

  myseq [] = [[]]
  myseq  (list : lists) =
    [ x : xs
    | x <- list
    , xs <- myseq lists
    ]

  accordingClump :: [Clump] -> Pair -> Clump
  accordingClump dat pair = dat !! idx
    where idx = indexOfClumpPair pair dat 0

  getAdjacentClumps ::  [Clump] -> Pair -> Size -> [Clump]
  getAdjacentClumps dat (c, r)  d
    | equalPairs (c,r) (0,0) = [below, right]
    | equalPairs (c,r) (0,(d - 1)) = [above, right]
    | equalPairs (c,r) ((d - 1),0) = [below, left]
    | equalPairs (c,r) ((d - 1),(d - 1)) = [above, left]
    | ((c == (d-1)) && noExtRow)  = [above, below, left]
    | ((c == 0) && noExtRow) = [above, below, right]
    | ((r == (d - 1)) && noExtCol)  = [above, left, right]
    | ((r == 0) && noExtCol) = [below, left, right]
    | otherwise = [above, below, left, right]
    where right = accordingClump dat ((c+1), r)
          left = accordingClump dat ((c-1), r)
          below = accordingClump dat (c, (r+1))
          above = accordingClump dat (c, (r-1))
          noExtRow = ((r /= 0) && (r /= (d - 1)))
          noExtCol = ((c /= 0) && (c /= (d - 1)))


  rmFromAdjacent :: [Clump] -> Target ->  [Clump]
  rmFromAdjacent dat target = lUpd
      where lCoord = fst (unzip dat)
            lVal = head (snd (unzip dat))
            filt = foreach lVal 0 target []
            lUpd = zip lCoord [filt]

            foreach :: [[N]] -> Idx -> Target -> [[N]] -> [[N]]
            foreach vals idx targ accum 
              | (idx < (length vals)) && ((targ `elem` this) && (length this) > 1) = foreach vals (idx+1) targ accum
              | (idx < (length vals)) && (targ `notElem` this) = foreach vals (idx+1) targ (accum++[this])
              | otherwise = accum
                where this = vals !! idx
  
  updateClumps :: [Clump] -> [Clump] -> [Clump] -> [Clump]
  updateClumps _ [] final = final
  updateClumps initList updated final = updateClumps new (tail updated) new
    where
      fChunk = (head updated)
      fElem = fst fChunk
      pidx = indexOfClumpPair fElem initList 0
      spl = splitAt (pidx) initList
      new = (fst spl) ++ [fChunk] ++ (tail (snd spl))

                              
  prClump :: [((N,N), [[N]])]  -> Size -> [((N,N), [[N]])] 
  prClump dat size = looper dat [] 0 size
    where
      looper dat accum idx size 
        | (idx < (length dat)) && ((length filt) >= 1) = looper upd upd (idx + 1) size
        | (idx < (length dat)) && ((length filt) < 1) = looper dat accum (idx + 1) size
        | (length accum) == 0 = dat
        | otherwise = accum
        where 
          this = dat !! idx
          c = fst (fst this)
          r = snd (fst this)
          nums = snd this
          filt = filter ((==1) . length ) nums
          upd = lookAtAdj dat (c,r) size (head (head nums))

          lookAtAdj datt (col,row) size target = updateClumps datt rmAdj []
            where
              adj = getAdjacentClumps datt (col,row) size
              rmAdj = rmFromAdjacent adj target                              


  pr :: [Chunk] -> Size -> [Chunk]
  pr dat size = looper dat [] 0 size
    where
      looper dat accum idx size 
        | (idx < (length dat)) && ((length nums) == 1) = looper dat upd (idx + 1) size
        | (idx < (length dat)) && ((length nums) > 1) = looper dat accum (idx + 1) size
        | (length accum) == 0 = dat
        | otherwise = accum
        where 
          this = dat !! idx
          c = fst (fst this)
          r = snd (fst this)
          nums = snd this
          upd = lookAtAdj dat (c,r) size (head nums)

          lookAtAdj datt (col,row) size target = updateNew datt rmAdj []
            where
              adj = getAdjacent datt (col,row) size
              rmAdj = removeFromAdjacent adj target

  cn :: Foldable t => t [a] -> [a]
  cn = concat

  swapper :: [Chunk] -> [Chunk]
  swapper [] =[]
  swapper (x:xs) = ((snd (fst x),fst (fst x)), (snd x)) : (swapper xs)


  clumpSwap :: [Clump] -> [Clump]
  clumpSwap clumps = looping clumps  0 []
    where
      looping clump idx accum
        | (idx < length (clump)) = looping clump (idx+1) (accum ++ [((c,r),d)])
        | otherwise = accum
        where this = clump!!idx 
              r = fst (fst this)
              c = snd (fst this)
              d = snd this


  fseq :: [[N]] -> Size -> IO [[N]]
  fseq nums size = do 
    let quence = nums
    let slen = length quence
    passed <- parsing quence slen 0 [] size
    return passed
    where 
          parsing :: [[N]] -> Len -> Idx -> [[N]] -> Size -> IO [[N]] 
          parsing sq slen idx accum dimen = do
            if (idx < slen) 
              then do
                let this = sq !! idx
                    chunked = chunkUp dimen this
                    chunkedcols = transpose chunked
                    len = length chunked
                passedCheck <- checkchunks chunked chunkedcols 0
                if (passedCheck) 
                  then do
                    parsing sq slen (idx + 1) (accum ++ [this]) dimen
                  else if (not (passedCheck)) 
                    then do
                      parsing sq slen (idx + 1) (accum) dimen
                    else do
                      return accum 
              else do
                return accum      
                
            where 
              checkchunks cr cc cidx = do
                if (cidx < length cr)
                 
                  then do
                    --ps (cn [s (length cr)," ", s (length cc), " ", s cidx ])
                    let thisrow = cr !! cidx
                        thiscol = cc !! cidx
                        nubbedrow = nub thisrow
                        nubbedcol = nub thiscol
                        diff1 = (length thisrow) - (length nubbedrow)
                        diff2 = (length thiscol) - (length nubbedcol) 
                    if ((diff1 /= 0) || (diff2 /= 0))
                      then do 
                        return False
                      else if ((diff1 == 0) && (diff1 == 0))
                        then do
                          checkchunks cr cc (idx+1)
                        else do 
                          return False
                  else do 
                    return False

  initGrid :: Idx -> [Chunk] -> Row -> Row
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
           
  dispFullGrid :: Grid -> Size -> String
  dispFullGrid grid size =  unlines (map (unwords . map showCell) grid)
    where
      showCell (F x)
        | size == 4 = "[" ++ show x ++ "   ]"
        | size == 5 = "[" ++ show x ++ "    ]"
        | size == 6 = "[" ++ show x ++ "     ]"
      showCell (P xs) = (++ "]") . foldl' (\acc x -> acc ++(if x `elem` xs then show x else " ")) "[" $ [1..size]
  
  
  weedOut :: [Cell] -> Maybe [Cell]
  weedOut cells = traverse checkSpots cells
    where constants = [lone | F lone <- cells]
          checkSpots lone = Just lone
          checkSpots (P group)
            | null ldiv = Nothing
            | len == 1 = Just (F (head group))
            | len > 1 = Just (P group)
            where ldiv = group \\ constants
                  len = length ldiv
 
  isFinal :: Cell -> Bool
  isFinal (F _) = True
  isFinal _ = False

  isPoten :: Cell -> Bool
  isPoten (P _) = True
  isPoten _ = False

  dispGrid :: Grid -> String
  dispGrid = unlines . map (unwords . map showCell)
    where showCell (F x) = show x
          showCell _ = "-"

  getAdjacent ::  [Chunk] -> Pair -> Size -> [Chunk]
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

  removeFromAdjacent :: [Chunk] -> Target -> [Chunk]
  removeFromAdjacent dat target = lUpd ++ shortGuys
      where longerVals = filter ((> 1) . length . snd) dat -- ensure list with 1 item does not have val removed
            shortGuys = filter ((== 1) . length . snd) dat
            lCoord = fst (unzip longerVals)
            lVal = snd (unzip longerVals)
            filt = map (filter (/= target)) lVal
            lUpd = zip lCoord filt

  updateNew :: [Chunk] -> [Chunk] -> [Chunk] -> [Chunk]
  updateNew _ [] final = final
  updateNew initList updated final = updateNew new (tail updated) new
    where
      fChunk = (head updated)
      fElem = fst fChunk
      pidx = indexOfPair fElem initList 0
      spl = splitAt (pidx) initList
      new = (fst spl) ++ [fChunk] ++ (tail (snd spl))

  mkEmpty :: Size -> Count -> [[N]]
  mkEmpty n 0 = []
  mkEmpty n count = [(take n [0,0..])] ++ mkEmpty n (count - 1)

  bigLineTime :: Show a => [[a]] -> String 
  bigLineTime line = concat (map lineTime line)

  lineTime :: Show a => [a] -> String
  lineTime line = ((s line) ++ "\n\n")

  noInnerDupes :: [[N]] -> Bool
  noInnerDupes [] = True
  noInnerDupes list
    | (length chker) /= (length this) = False
    | otherwise = noInnerDupes shorter
    where this = head list
          chker = nub this
          shorter = tail list
           
  smush :: [[[N]]] -> Idx -> [[N]] -> [[N]]
  smush grid idx accum
    | idx < (length grid) = smush grid (idx + 1) (accum ++ [tmpCon])
    | otherwise = accum
    where tmp  = grid !! idx
          tmpCon = concat tmp

  rearrange :: [[N]] -> Idx -> [[N]] -> [[N]]
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

  iinsert :: N -> a -> [a] -> [a]
  iinsert n y xs = countdown n xs where
    countdown 0 xs = y:countdown n xs -- reset to original n
    countdown _ [] = []
    countdown m (x:xs) = x:countdown (m-1) xs

  according :: [Chunk] -> Pair -> Chunk
  according dat pair = dat !! idx
    where idx = indexOfPair pair dat 0

  allSameLength :: [[a]] -> Bool
  allSameLength []     = True
  allSameLength (noggin : body) = all (\inner -> length inner == length noggin) body

  equalPairs :: Pair -> Pair -> Bool
  equalPairs (fstCol, fstRow) (sndCol, sndRow) = ((fstCol == sndCol) && (fstRow == sndRow))

  process :: [Flne] -> Idx -> Count -> Size -> [(Mark, N)] -> IO [(Mark, N)]
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


  process2 :: [Flne] -> Idx -> Count -> Size -> [(Mark, [[N]])] -> IO [(Mark, [[N]])]
  process2 line idx count size coords = do
    if count == 0
      then return coords
      else do
        let thisLn = (line !! idx)
            potenVals = findVals thisLn size
            target = getCageTarget thisLn
            worded = (words thisLn)
        potPairs <- possiblePairs worded potenVals target
        --ps $ s 
        
        let chk = fst (unzip potPairs)
            noSize = tail worded
            sizeOnly = read (head worded) :: N
            noEnd = if (sizeOnly == 1) then init noSize else init (init noSize)
            otherway = swapname noEnd
            zipped = customZip otherway potenVals
        ps $ s (length noEnd)
        --ps $ s noEnd
    
        process2 line (idx + 1) (count - 1) size (coords ++ zipped)

  customZip :: [Mark] -> [[N]] -> [(Mark, [[N]])]
  customZip mks nums = for mks nums
    where
      for marks [] = []
      for [] vals  = []
      for [mark] vals = [(mark, vals)]  
      for marks vals  = [(thisMk, vals)] ++ (for (tail marks) vals)
        where thisMk = head marks
             
  
  swapname :: [Mark] -> [Mark]
  swapname [] = []
  swapname info = [new] ++ swapname (tail info)
    where this = head info
          fp = head this
          sp = last this
          new = [sp]++[fp]


  customUnzip :: [(Mark, [[N]])] -> ( [Mark], [[[N]]] ) 
  customUnzip dat = (newmks, newnums)
    where newmks = formks dat
          newnums = stuff dat
          stuff [] = []
          stuff info = [znums] ++ (stuff (tail info))
            where cur = head info
                  n = snd cur 
                  znums = fornums n
                  fornums [] = []
                  fornums sinfo = [this] ++ (fornums (tail sinfo))
                    where this = head sinfo
          formks [] = []
          formks info = [mks] ++ (formks (tail info))
            where this = head info
                  mks = fst this



  possiblePairs :: [Mark] -> [[N]] -> Target -> IO [(Mark, N)]
  possiblePairs str vals target = do
    let size = (read (head str)) :: N
    if size == 1
      then return [((str !! 1), target)]
      else do
        coordPairs <- outerPairer (permutations (init (init (tail str)))) vals [] 0
        return (coordPairs)
        --------------------------------------------------------------------------
        where 
          outerPairer str vals accum idx = do
            if (length vals) == idx
              then return (accum)
              else do
                pairOne <- innerPairer str (vals !! idx) [] 0
                outerPairer str vals (accum ++ pairOne) (idx + 1)
                -------------------------------------------------------------------
                where 
                  innerPairer str vals accum idx = do
                    if (length str) == idx
                      then return (accum)
                      else do innerPairer str vals (accum ++ (zip (str !! idx) vals)) (idx + 1)

  doSub :: String -> Size -> [[N]]
  doSub chars maxNum
    | cage > 3 = helper (getCageTarget chars) (findSubsets maxNum (cage-1)) []
    | otherwise = helper (getCageTarget chars) (findSubsets maxNum cage) []
    where cage = (digitToInt (head (filter isDigit chars)))
          -----------------------------------------------------------------------
          helper target [] accum = accum
          helper target sets accum = helper target (dropAt sets 0) updated
            where updated = accum ++ (subFold target ([(head sets)]) [])
                  ---------------------------------------------------------------
                  subFold target [] valid = valid
                  subFold target perms valid
                    | total == target = subFold target lessPerms (valid ++ [this])
                    | otherwise = subFold target lessPerms valid
                    where this = head perms
                          lessPerms = filter (/= this) perms
                          total = abs (foldl (-) (head this) (tail this))

  doAdd :: String -> Size -> [[N]]
  doAdd chars maxNum
    | cage > 3 = helper (findSubsets maxNum (cage)) [] (getCageTarget chars) 0
    | otherwise = helper (findSubsets maxNum cage) [] (getCageTarget chars) 0
    where cage = (digitToInt (head (filter isDigit chars)))
          ----------------------------------------------------------------------------
          helper [] accum target idx = accum
          helper list accum target idx
            | (li list) == idx = accum
            | ((sum (list !! idx)) == target) && ((li list) /= idx) = 
                  helper list (accum ++ [(list !! idx)]) target (idx + 1)
            | ((sum (list !! idx)) /= target) && ((li list) /= idx) = 
                  helper list accum target (idx + 1)

  findSubsets :: Size -> Size -> [[N]]
  findSubsets maxNum cageSize = sort (uniqLists (subGen cageSize legal) 0)
    where legal = repConcat (legalNums maxNum) cageSize
          ----------------------------------------------------------------
          legalNums 0    = []
          legalNums size = sort ([size] ++ (legalNums (size - 1)))
          ----------------------------------------------------------------
          subGen 0 _ = [[]]
          subGen _ [] = []
          subGen lim (noggin : body) = [noggin : subs | subs <- subGen (lim - 1) body] ++ subGen lim body
          ----------------------------------------------------------------
          repConcat list times
            | times > 3 = sort (concat (replicate (times - 2) list))
            | otherwise = sort (concat (replicate times list))

  indexOf :: Eq a => a -> [a] -> Idx -> Idx
  indexOf item arr idx
    | item == (arr !! idx) = idx
    | item /= (arr !! idx) = indexOf item arr (idx + 1)
    | otherwise = -1

  indexOfPair :: Eq a => (a, a) -> [((a, a), [a])] -> Idx -> Idx
  indexOfPair item arr idx
    | ((length arr) > idx) && (item == thisPair) = idx
    | item == thisPair = idx
    | item /= thisPair = indexOfPair item arr (idx + 1)
    | otherwise =   -1
    where thisPair = (fst (arr !! idx)) 

  indexOfClumpPair :: Eq a => (a, a) -> [((a, a), [[a]])] -> Idx -> Idx
  indexOfClumpPair item arr idx
    | ((length arr) > idx) && (item == thisPair) = idx
    | item == thisPair = idx
    | item /= thisPair = indexOfClumpPair item arr (idx + 1)
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

  uniqLists :: [[N]] -> Idx -> [[N]]
  uniqLists [] idx = []
  uniqLists list idx
    | idx < (li list) = uniqLists ([tmpList] ++ (filter (/= tmpList) list)) (idx + 1)
    | otherwise = list
    where tmpList = (list !! idx)

  --groupValues :: [(Mark, N)] -> [(Mark, [N])]
  

  chunkUp :: Show a => Idx -> [a] -> [[a]]
  chunkUp _ [] = []
  chunkUp count ns =
    let (os, ps) = splitAt count ns
      in os : chunkUp count ps

  dropAt :: [a] -> Idx -> [a]
  dropAt list idx = take idx list ++ drop (idx + 1) list

  li :: [a] -> N
  li str = (length str) - 1

  form :: (Read a, Read b, Show a, Show b) => Len -> Count -> [(a, b)] -> String -> String
  form len flag [] accum = accum
  form len flag pairs accum = formed
    where coord = s (fst (head pairs))
          num = s (snd (head pairs))
          out = concat [accum, coord, " --> ", num, (xsp (length num)) ,"  ", (fst $ es flag) ]
          cut = drop 1 pairs
          formed = form len (snd $ es flag) cut out
          es 1   = ("\n", brfl)
          es cnt = ("  ", (cnt - 1))
          xsp sze = take (((len+1) - (sze)) + len) [' ',' '..]

  formGrid :: [[[N]]] -> Idx -> String -> String
  formGrid grid idx accum
    | ((li grid) + 1) == idx = accum ++ "\n"
    | otherwise = formGrid grid (idx + 1) (accum ++ (s (grid !! idx)) ++ "\n")

  listifyGrid :: [(a, [N])] -> Idx -> [[N]] -> [[N]]
  listifyGrid coordVals idx accum
    | (li coordVals) == idx = accum ++ vals
    | otherwise = listifyGrid coordVals (idx + 1) (accum ++ vals)
    where
      vals = [snd (coordVals !! idx)]

  findVals :: Flne -> Size -> [[N]]
  findVals list size
    | (last list) == '-' = doSub list size
    | (last list) == '+' = doAdd list size
    | otherwise = [[(digitToInt (last list))]]

  colRowPair :: (Mark, [N]) -> Chunk
  colRowPair coords = ((numCol, irow), (snd coords))
    where irow = ((read [(fst coords) !! 1]) - 1)
          numCol = ((indexOf (head (fst coords)) colNames 0))

  rowColPair :: (Mark, [N]) -> Chunk
  rowColPair coords = ((irow, numCol), (snd coords))
    where irow = ((read [(fst coords) !! 1]) - 1)
          numCol = ((indexOf (head (fst coords)) colNames 0))

  rowColClump :: (Mark, [[N]]) -> ((N,N), [[N]])
  rowColClump clump = ((irow, numCol), (snd clump))
    where point = (fst clump)
          ir = digitToInt (head point)
          irow = (ir - 1)
          numCol = (indexOf (last point) colNames 0)

  type Count = Int
  type Idx = Int
  type Len = Int

  type Size = Int

  type N = Int



  type Step = Int
  type Target = Int


  type Flne = String
  type Mark = String
  