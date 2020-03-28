  {-# LANGUAGE ExplicitForAll #-}
  {-# OPTIONS_GHC 
      -fbreak-on-exception
      -fprof-auto 
      -fprof-cafs
      -rtsopts
      -ferror-spans
    
  #-}
  ---Wunused-imports
  module Main where

  import Data.Char
  import Data.List
  import Data.List.Split
  import System.Environment
  import Data.Function
  import Data.Typeable
  import Data.Ord
  import Data.Map (fromListWith, toList)






  
  type Pair = (N,N)
  type Chunk = ((N, N), [N])
  type Clump = ((N,N), [[N]])
  type C = Int
  type I = Int
  type Len = Int
  type Size = Int
  type N = Int
  type Step = Int
  type Target = Int
  type Pos = String
  type Lbl = String
  type Cage = [(Lbl, ([Pos], [[N]]))]


  brfl = 4
  colNames = "abcdef"
  dlbl = "dbug"
  nl = "\n"
  sp = " "
  psl str = putStrLn str
  sh val = show val
  ln val = length val
  ct val = concat val
 


  main :: IO ()
  main = do
    fName <- getArgs
    let fNoRet = filter (/= '\r') (fName !! 0)
    fData <- readFile fNoRet
    let fLines = lines fData
        fLineNoRet = map (filter (/= '\r')) fLines
        dimens = read (fLineNoRet !! 0) :: N -- passed

    psl ("Board size: " ++ (show dimens))

    proc2 <- processData (tail fLineNoRet) 0 (length (tail fLineNoRet)) dimens []
    let unzipped = chunkUp dimens (snd (customUnzip (sort proc2)))
        swapped = transpose unzipped
        byRow =  sort (map rowColClump (sort proc2) )
        cleanedByRow = cleanUp byRow dimens

        byCol = sort (clumpSwap cleanedByRow)
        cleanedByCol = cleanUp byCol dimens
        backToRow = sort (clumpSwap cleanedByCol)
        
    slmd <- slimDown proc2 0 []
    psl nl
    let slmSort =  map (sortBy (compare `on` (length . snd))) (groupBy ((==) `on` fst) slmd)
        smallerslm = map head slmSort
    
    psl nl
    cs <- cageStuff smallerslm
    plocs <- possibleLocs cs 
   
    psl nl
    let tst = zip (map fst plocs) (map (groupBy ((==) `on` fst)) ((map snd plocs)))
    oke <- okay tst
    chkdv <- checkViolations oke
    psl nl
    let uh = (map show chkdv)
        uh2 = (map show oke)
    -- print tst
    psl nl
    pnl uh
  

  checkViolations :: [(Lbl,[[(Pos,Int)]])] -> IO[(Lbl,[[(Pos,Int)]])]
  checkViolations combos = do
    newCages <- forEachCage combos 0 []
    return newCages
    where
      forEachCage :: [(Lbl,[[(Pos,Int)]])] -> I -> [(Lbl,[[(Pos,Int)]])] -> IO [(Lbl,[[(Pos,Int)]])]
      forEachCage combos i accum = do
        if (i == (ln combos)) then return accum
        else if ((ln (combos!!i)  ) == 1) then do
          forEachCage combos (i+1) (accum++[(combos!!i)])
        else do
          let thisCage = combos !! i
              lbl = fst thisCage
              dat = snd thisCage
          withUpdated <- forEveryCombo  dat 0 []
          let updat = [(lbl, withUpdated)]
          forEachCage combos (i+1) (accum++updat)
        where
          forEveryCombo :: [[(Pos,Int)]] -> I -> [[(Pos,Int)]] -> IO [[(Pos,Int)]]
          forEveryCombo combs j accum2 = do
            if (j == (ln combs)) then do
              psl "done" 
              return accum2
            else do
              let thisComb = combs !! j
                  points = fst (unzip thisComb)
              print points
              let fstPt = (head points)
                  allsame = all ((== fstPt) . fst) thisComb
              let nums = snd (unzip thisComb)
                  isLine = cageIsLine points
                  freqs = frequency nums
                  det = filter ((1<) . snd ) freqs
              max <- maxDupe points

              if (allsame) then do
                forEveryCombo combs (j+1) accum2
              else do
                let nubbed = nub points
                pl ["before:",sh points,"nubbed:",sh nubbed]
                if (isLine) then do
                  if ((ln det) > 0) then do forEveryCombo combs (j+1) accum2
                  else do forEveryCombo combs (j+1) (accum2 ++ [thisComb])
                else do
                  if ((ln det) > max) then do forEveryCombo combs (j+1) accum2 
                  else do forEveryCombo combs (j+1) (accum2 ++ [thisComb])

            
  frequency :: (Ord a) => [a] -> [(a, Int)]
  frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])         
    
  okay :: [(Lbl,[[(Pos,Int)]])] -> IO[(Lbl,[[(Pos,Int)]])]
  okay stuff = do
    cagecombos <- forEachCage stuff 0 []
    return cagecombos
    where
      ---------------------------------------------------------------------------------
      -- loop is for each cage
      forEachCage :: [(Lbl,[[(Pos,Int)]])] -> I -> [(Lbl,[[(Pos,Int)]])] -> IO[(Lbl,[[(Pos,Int)]])]
      forEachCage poss i accum1 = do --for each cage   
        -- pl ["poss:", sh poss]
        -- pl ["len poss:", shl poss]
        if (i == (ln poss)) then do
          --pl ["accum1:", sh accum1]
          print i 
          return accum1
        else do
          print i
          let thisCage = poss !! i
          -- pl ["thiscage:", sh thisCage]
          let id = fst thisCage 
          picked <- pickOne (snd thisCage) 0 []
          let pickLess = filter ((0<). length) picked
          let updat = [(id, pickLess)]
          --pl ["updat:", sh updat] 
          forEachCage poss (i+1) (accum1++updat)
          where
            ---------------------------------------------------------------------------------
            -- loop through each list of coord-val pairs in the cage. ex. like [(a1,2),(a1,3)]...
            pickOne :: [[(Pos,Int)]] -> I -> [[(Pos,Int)]] -> IO[[(Pos,Int)]]
            pickOne list1 j accum2 = do --for each list of cells
              -- pl ["list1:", sh list1]
              if ((j == (ln list1))) then do
                return accum2
              else if ((ln list1) == 1) then do
                -- pl ["list1:", sh list1]
                pickOne list1 (j+1) (accum2++list1) 
                -- return list1
              else do
                let thisPair = list1 !! j
                picked <- forEachPoint thisPair list1 0 []
                -- pl ["picked:", sh picked] 
                pickOne list1 (j+1) (accum2++picked)
                where
                ---------------------------------------------------------------------------------
                -- for each possible value point, pair look at each possible points from the cage
                forEachPoint :: [(Pos,Int)] -> [[(Pos,Int)]] -> I -> [[(Pos,Int)]] -> IO [[(Pos,Int)]]
                forEachPoint pts others k accum3 = do
                  if (k == (ln pts)) then do
                    --pl ["accum3:", sh accum3]  
                    return accum3
                  else do
                    let thisPt = pts !! k
                    matchedSets <- forEachOtherSet thisPt others 0 []
                    forEachPoint pts others (k+1) (accum3++matchedSets)
                    where
                    ---------------------------------------------------------------------------------
                    -- for each possible value point explore all
                      forEachOtherSet :: (Pos,Int) -> [[(Pos,Int)]] -> I -> [[(Pos,Int)]] -> IO [[(Pos,Int)]]
                      forEachOtherSet point sets m accum4 = do
                        if (m == (ln sets)) then do
                          -- pl ["accum4:", sh accum4] 
                          return accum4
                        else do
                          let thisSet = sets !! m 
                          if(((ln thisSet) == (ln [point])) || ((ln thisSet) == 0)) then do
                            -- psl "(((ln thisSet) == (ln [point])) || ((ln thisSet) == 0)) "
                            forEachOtherSet point sets (m+1) (accum4++[[point]])
                          else do
                            matched <- matchEm point thisSet
                            forEachOtherSet point sets (m+1) (accum4++[(nub(sort matched))])
                          where
                          ---------------------------------------------------------------------------------
                          -- finally for each singular point, actually match em up
                            matchEm :: (Pos, Int) -> [(Pos,Int)] -> IO [(Pos,Int)]
                            matchEm pt smSet = do
                              -- psl "-------------------------------------------------------------"
                              -- pl ["pt:",sh pt,"smSet:",sh smSet]
                              if ((ln smSet) == 1) then do 
                                -- psl "smSet has len 1" 
                                return smSet
                              else do
                                let setCoords = (map fst smSet)
                                    thisCoord = fst pt
                                    chunks = chunksOf 1 (smSet)
                                    added = (concat (map (pt:)chunks))
                                -- pl ["added:",sh added] 
                                return added
      


  inSameLine :: Pos -> Pos -> Bool
  inSameLine p1 p2
    | ((c1 == c2) || (r1 == r2)) = True
    | otherwise = False 
    where (c1,c2,r1,r2) = (head p1,head p2,last p1, last p2)
      
  possibleLocs :: [(Lbl, ([Pos], [N]))] -> IO [(Lbl, [(Pos,N)])]
  possibleLocs poss = do 
    possible <- forEveryCage poss 0 []
    return possible
    where
      forEveryCage line i accum = do
        if (i == (ln line)) then return accum
        else do 
          let this = line !! i 
              (lbl, dat, pts, nums) = (fst this,snd this, fst dat, snd dat)
          pairs <- forEveryPoint pts nums 0 []
          forEveryCage line (i+1) (accum++[(lbl, pairs)])
          --------------------------------------------------------
          where
            forEveryPoint points nums j  accum = do
              if (j == (ln points)) then return accum
              else do
                let thisPt = points !! j
                combos <- forEveryNum thisPt nums 0 []
                forEveryPoint points nums (j+1) (accum ++ combos)
                ----------------------------------------------------
                where
                  forEveryNum point nums k accum = do
                    if (k == (ln nums)) then return accum
                    else do
                      let thisNum = nums !! k
                      forEveryNum point nums (k+1) (accum ++ [(point,thisNum)])


  cageStuff :: [(Pos, [[N]])] -> IO [(Lbl, ([Pos], [N]))] -- cage has list of coordinates & values
  cageStuff chunks = do
    fName <- getArgs
    fData <- readFile (fName !! 0) -- passed?
    let fLines = map (filter (/= '\r')) (lines fData)
        noOps = map (filter (/= '+')) (map (filter (/= '-')) (tail fLines))
        (cages,worded) = (map digitToInt (map head noOps), map words noOps)
        (points,dataSize) = (map tail (map init (map words noOps)), ln points)
    labeled <- forLn chunks 0 []
    let grped = sort (groupBy ((==) `on` fst) (sort labeled))
    asCages <- forEachUid grped 0 []
    return asCages
      where  forLn dat i accum = do
                if (i == (ln dat)) then return accum
                else do
                  let (this, point, nums) = ((dat !! i),(fst this), (snd this))
                  cageLbl <- getCage point
                  forLn dat (i+1) (accum ++ [(cageLbl, (point, nums ))])
             forEachUid :: [[(Lbl, (Pos, [[N]]))]] -> I -> [(Lbl, ([Pos], [N]))] -> IO [(Lbl, ([Pos], [N]))]
             forEachUid cData i accum = do
                if (i == (ln cData)) then return accum
                else do 
                  let this = cData !! i
                  let unz = unzip this
                      lbl = head (fst (unzip this))
                  let dat = snd unz
                      pts = map fst dat
                      nums = nub(concat (head ((snd(unzip dat)))))
                  forEachUid cData (i+1) (accum ++ [(lbl, (pts,nums))]) 

  getCage :: Pos -> IO Lbl
  getCage point = do
    fName <- getArgs
    fData <- readFile (fName !! 0) 
    let fLines = map (filter (/= '\r')) (lines fData)
        noOps = map (filter (/= '+')) (map (filter (/= '-')) (tail fLines))
        (cages, worded) =( map digitToInt (map head noOps), map words noOps)
        (points,dataSize) = (map tail (map init (map words noOps)),ln points)
        num = map show [1..dataSize]
        identifiers = map ("C"++) num
        cagePoints = zip identifiers points -- type [(String,[Pos])]

    members <- findCage cagePoints 0 point []
    return members
    where
      findCage :: [(String,[Pos])] -> I -> Pos -> Pos -> IO String 
      findCage lns i toMatch holder = do
            let done = (i == (ln lns))
            if done then return holder
            else do
              let thisln = lns !! i
                  (iden, points) = (fst thisln, snd thisln)
              if ([toMatch] `isInfixOf` points) then do findCage lns (i+1) toMatch iden
              else do
                let swapped = [(last toMatch)]++[(head toMatch)]
                if ([swapped]  `isInfixOf` points) then do findCage lns (i+1) toMatch iden
                else do findCage lns (i+1) toMatch holder 




    
  getCageMembers :: Pos -> IO [Pos]
  getCageMembers point = do
    fName <- getArgs
    fData <- readFile (fName !! 0) 
    let fLines = map (filter (/= '\r')) (lines fData)
        noOps = map (filter (/= '+')) (map (filter (/= '-')) (tail fLines))
        (cages, worded) =( map digitToInt (map head noOps), map words noOps)
        points = map tail (map init (map words noOps))
    members <- findMembers points 0 point []
    return members
    where
      findMembers :: [[Pos]] -> I -> Pos -> [Pos] -> IO [Pos] 
      findMembers lns i toMatch holder = do
            let done = (i == (ln lns))
            if done then return holder
            else do
              let thisln = lns !! i
              if ([toMatch] `isInfixOf` thisln) then do findMembers lns (i+1) toMatch thisln
              else do
                let swapped = [(last toMatch)]++[(head toMatch)]
                if ([swapped]  `isInfixOf` thisln) then do findMembers lns (i+1) toMatch thisln
                else do findMembers lns (i+1) toMatch holder

  isInLine :: Pos -> IO Bool
  isInLine point = do
    members <- getCageMembers point
    if (members == [""]) then return False
    else do
      let cage = cageIsLine members
      return cage

  maxDupe :: [Pos] -> IO N
  maxDupe points = do 
    if (((ln points) < 3) || (cageIsLine points)) then return 1
    else do
      let (cols,rows) = ((group (sort (map (take 1) points))), (group (sort (map (drop 1) points))))
          (sCols,sRows) = (sortBy (compare `on` ln) cols, sortBy (compare `on` ln) rows)
          (scLen, srLen) = ( ln sCols , ln sRows)
      if ((scLen > srLen) || (scLen == srLen)) then return srLen
      else do return srLen
             

  getMaxDupes :: Pos -> IO N
  getMaxDupes point = do
    members <- getCageMembers point
    let (cols,rows) = ((group (sort (map (take 1) members))), (group (sort (map (drop 1) members))))
        (sCols,sRows) = (sortBy (compare `on` ln) cols, sortBy (compare `on` ln) rows)
        (scLen, srLen) = ( ln sCols , ln sRows)
    if ((scLen > srLen) || (scLen == srLen)) then return srLen
    else if ((ln members) < 3) then return 1
    else do return srLen
          

  slimDown :: [(Pos, [[N]])] -> I -> [(Pos, [[N]])] -> IO [(Pos, [[N]])]
  slimDown options i accum = do 
    if (i == ln options) then return (sort (nub accum))
    else do
      let (this, nums, mk) = ((options !! i),(snd this),(fst this))
          (row, col) = ((head mk),(last mk))
          flattened = concat nums
          thisCol = filter (((==col).last).fst) options
          thisRow = filter (((==row).head).fst) options    
      if ((ln flattened) == 1) then do -- this value is set
          updatRow <- forEachLine thisRow 0 (head flattened) []
          updatCol <- forEachLine thisCol 0 (head flattened) []
          let (rNums, cNums, upRNums, upCNums) = (snd (unzip thisRow), snd (unzip thisCol), snd (unzip updatRow), snd (unzip updatCol)) 
          slimDown options (i+1) (accum++updatRow++updatCol)
      else do
        slimDown options (i+1) (accum++thisRow++thisCol)       
        where forEachLine :: [(Pos, [[N]])] -> I -> N -> [(Pos, [[N]])] -> IO [(Pos, [[N]])]
              forEachLine items j v accum = do
                let done = (j == (ln items))
                if (done) then return accum
                else do
                  let item = items !! j
                      (coord, nums) = (fst item, snd item)
                      numHeads = head nums
                      longEnough = (((ln nums) > 1))
                  --pl ["coord", coord]
                  changedSet <- (forEachSet nums coord 0 v [])
                  let changed = (coord, changedSet)
                  if (longEnough) then do forEachLine items (j+1) v (accum++[changed])
                  else do forEachLine items (j + 1) v (accum ++ [item])
                where forEachSet :: [[N]] -> Pos -> I -> N -> [[N]] -> IO [[N]]
                      forEachSet vals pt k d accum = do
                        let done = k == (ln vals)
                        if done then return accum
                        else do
                          let (thisSet, hasNum, longEnough) = ((vals !! k), (d `elem` thisSet), ((ln thisSet)>1))
                          maxx <- getMaxDupes pt
                          --pl [sh maxx, pt]
                          inLine <- isInLine pt
                          if (hasNum && longEnough && (not inLine)) then do
                            let occur = filter (==d) thisSet
                            if ((ln occur) < maxx) then do 
                              if ((k == (li vals)) && ((ln accum) == 0)) then do forEachSet vals pt (k+1) d (accum++[thisSet])
                              else do  forEachSet vals pt (k+1) d accum
                            else do forEachSet vals pt (k+1) d (accum++[thisSet])
                          else if (hasNum && longEnough) then do forEachSet vals pt (k+1) d accum
                          else do forEachSet vals pt (k+1) d (accum++[thisSet])
                            
  cageIsLine :: [Pos] -> Bool
  cageIsLine points = ((ln colGrps) == 1 || (ln rowGrps) == 1) -- if this holds true then the cage is a line
    where colGrps = group (sort (map (take 1) points)) 
          rowGrps = group (sort (map (drop 1) points))
      

  cageChk :: (N,N) -> (N,N) -> (Target,Target) -> (Char,Char) -> Bool
  cageChk (0,b) (c,d) (e,f) (g, h) = ((c==d) && (e==f) && (g==h))
  cageChk (a,b) (c,d) (e,f) (g, h) = ((a==b) && (c==d) && (e==f) && (g==h))

  hasMoreThan :: N -> [N] -> Bool
  hasMoreThan max list 
    | (ln biggestDupes) < max = False
    | (ln biggestDupes) > max = True
    | otherwise = False
    where gs = group (sort list)
          sorted = sortBy (comparing length) gs
          biggestDupes = (last sorted)

  
  applyCageRules :: [[N]] -> [Pos] -> Size ->  Size -> Target -> Char -> IO [[N]]
  applyCageRules    nums     points   squares  dimens  target    oper = do
    if ((squares < 3)) then do
        return nums
      else do
        let (cols, rows) = ((map (take 1) points), (map (drop 1) points))
        maxx <- getMaxDupes ((head cols)++(head rows))
        let (colGrps,rowGrps) = ((group (sort cols)), (group (sort rows)))
            (cgLen,rgLen ) = ((ln colGrps),(ln rowGrps))
            (sq, tg, op, dm) = (squares, target, oper, dimens)
            isLine = cageIsLine points
            checkValues poss i accum
              | (again && (not passed) && isLonger) = checkValues (tail poss) (i+1) accum
              | (again && atEnd && (not passed) && (blank)) =  checkValues (tail poss) (i+1) (accum ++ [this])
              | (again && passed) =  checkValues (tail poss) (i+1) (accum ++ [this])
              | otherwise = accum
              where (this,nubbed) = (poss !! i, nub this)
                    passed = (not (hasMoreThan maxx this))
                    (atEnd, blank, again, isLonger) = (i == (li poss), ((ln accum) == 0), i < (ln poss),((ln this) > 1))

        -- strict rules for cages while searching for tips on solving KenKen, helps cleaning process
        if ((cageChk (0,dm) (3,sq)(6,tg)('+',op)) && (isLine)) then return [[1,2,3]]
        else if ((cageChk (0,dm) (3,sq) (7,tg) ('+',op)) && (isLine)) then return [[1,2,4]]
        else if (cageChk (0,dm) (3,sq) (4,tg) ('+',op)) then return [[1,1,4]]
        else if (cageChk (0,dm) (2,sq) (4,tg) ('+',op)) then  return [[1,3]]
        else if (cageChk (4,dm) (2,sq) (3,tg) ('-',op)) then return [[1,4]]
        else if (cageChk (4,dm) (2,sq) (6,tg) ('+',op)) then do return [[2,4]]
        else if (cageChk (4,dm) (2,sq) (7,tg) ('+',op)) then do return [[3,4]]
        else if (cageChk (5,dm) (2,sq) (4,tg) ('-',op)) then do return [[1,5]]
        else if (cageChk (5,dm) (2,sq) (9,tg) ('+',op)) then do return [[4,5]]
        else if (cageChk (5,dm) (2,sq) (8,tg) ('+',op)) then do return [[3,5]]
        else if (cageChk (6,dm) (2,sq) (5,tg) ('-',op)) then do return [[1,6]]
        else if (cageChk (6,dm) (2,sq) (11,tg) ('+',op)) then do return [[5,6]]
        else if (cageChk (6,dm) (2,sq) (10,tg) ('+',op)) then do return [[4,6]]
        else if ((cageChk (6,dm) (3,sq) (14,tg) ('+',op)) && (isLine)) then do return [[3,5,6]]
        else if (isLine) then do
            let updated = checkValues nums 0 []
            --pl ["post-update - isLine",sh points,sh updated, "max:", sh maxx]
            return updated
        else if (not isLine) then do
          let updated = checkValues nums 0 []
          --pl ["post-update - not line",sh points,sh updated,"max:", sh maxx]
          return updated
        else do return nums
         
  processData :: [Pos] -> I -> C -> Size -> [(Pos, [[N]])] -> IO [(Pos, [[N]])]
  processData line i c size coords = do
    if (i == (ln line))
      then return coords
      else do
        let thisLn = (line !! i)
        let potenVals = findVals thisLn size
        let target = getCageTarget thisLn
        let worded = (words thisLn)
        potPairs <- possiblePairs worded potenVals target
        let chk = fst (unzip potPairs)
        let noSize = tail worded
            sizeOnly = read (head worded) :: N
            noEnd = if (sizeOnly == 1) then init noSize else init (init noSize)
            otherway = swapname noEnd
        let letters = nub (fst (unzip potPairs))
            operation = head (last worded)
        newNums <- applyCageRules potenVals letters sizeOnly size target operation
        let zipped = customZip noEnd newNums
        --pl [sh otherway,sh newNums]           
        processData line (i + 1) (c - 1) size (coords ++ zipped)

  accordingClump :: [Clump] -> Pair -> Clump
  accordingClump dat pair = dat !! idx
    where idx = clumpPairIndex pair dat 0

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

            foreach :: [[N]] -> I -> Target -> [[N]] -> [[N]]
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
      pidx = clumpPairIndex fElem initList 0
      spl = splitAt (pidx) initList
      new = (fst spl) ++ [fChunk] ++ (tail (snd spl))
                              
  cleanUp :: [((N,N), [[N]])]  -> Size -> [((N,N), [[N]])] 
  cleanUp dat size = looper dat [] 0 size
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

  clumpSwap :: [Clump] -> [Clump]
  clumpSwap clumps = looping clumps 0 []
    where
      looping clump idx accum
        | (idx < length (clump)) = looping clump (idx + 1) (accum ++ [((c,r),d)])
        | otherwise = accum
        where this = clump!!idx 
              r = fst (fst this)
              c = snd (fst this)
              d = snd this

  sequenceCleaner :: [[N]] -> Size -> IO [[N]]
  sequenceCleaner nums size = do 
    let quence = nums
    let slen = length quence
    passed <- parsing quence slen 0 [] size
    return passed
    where 
          parsing :: [[N]] -> Len -> I -> [[N]] -> Size -> IO [[N]] 
          parsing sq slen idx accum dimen = do
            if (idx < slen) 
              then do
                --pl ["len sq", shl sq,"i",sh idx]
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
                    --pl ["cc len", shl cc,"cr len",shl cr, "cidx", sh cidx]
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

  equalPairs :: Pair -> Pair -> Bool
  equalPairs (fstCol, fstRow) (sndCol, sndRow) = ((fstCol == sndCol) && (fstRow == sndRow))

  customZip :: [Pos] -> [[N]] -> [(Pos, [[N]])]
  customZip mks nums = for mks nums
    where
      for marks [] = []
      for [] vals  = []
      for [mark] vals = [(mark, vals)]  
      for marks vals  = [(thisMk, vals)] ++ (for (tail marks) vals)
        where thisMk = head marks
             
  swapname :: [Pos] -> [Pos]
  swapname [] = []
  swapname info = [new] ++ swapname (tail info)
    where this = head info
          fp = head this
          sp = last this
          new = [sp]++[fp]

  customUnzip :: [(Pos, [[N]])] -> ( [Pos], [[[N]]] ) 
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

  possiblePairs :: [Pos] -> [[N]] -> Target -> IO [(Pos, N)]
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
    | cage > 3 = helper subsets [] cageTarget 0
    | otherwise = helper subsets [] cageTarget 0
    where cage = (digitToInt (head (filter isDigit chars)))
          cageTarget = getCageTarget chars
          subsets = findSubsets maxNum cage
          ---------------------------------------------------------------------------------
          helper [] accum target i = accum
          helper list accum target i
            | (li list) == i = accum
            | (thisSum == target) && ((li list) /= i) = helper list (accum ++ [(list !! i)]) target (i + 1)
            | (thisSum /= target) && ((li list) /= i) = helper list accum target (i + 1)
            where thisSum = sum (list !! i)

  
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

  basicIndexOf :: Eq a => a -> [a] -> I -> I
  basicIndexOf item arr idx
    | item == (arr !! idx) = idx
    | item /= (arr !! idx) = basicIndexOf item arr (idx + 1)
    | otherwise = -1

  pairIndex :: Eq a => (a, a) -> [((a, a), [a])] -> I -> I
  pairIndex item arr idx
    | ((length arr) > idx) && (item == thisPair) = idx
    | item == thisPair = idx
    | item /= thisPair = pairIndex item arr (idx + 1)
    | otherwise =   -1
    where thisPair = (fst (arr !! idx)) 

  clumpPairIndex :: Eq a => (a, a) -> [((a, a), [[a]])] -> I -> I
  clumpPairIndex item arr idx
    | ((length arr) > idx) && (item == thisPair) = idx
    | item == thisPair = idx
    | item /= thisPair = clumpPairIndex item arr (idx + 1)
    | otherwise =   -1
    where thisPair = (fst (arr !! idx)) 
               
  getCageTarget :: String -> Target
  getCageTarget str
    | cageSze == 1 = noOper
    | str == "" = 0
    | otherwise = target
    where wList = words str
          noOper = read (last wList) :: Target
          cageSze = read (head wList) :: Target 
          target = read (last (init wList)) :: Target

  uniqLists :: [[N]] -> I -> [[N]]
  uniqLists [] idx = []
  uniqLists list idx
    | idx < (li list) = uniqLists ([tmpList] ++ (filter (/= tmpList) list)) (idx + 1)
    | otherwise = list
    where tmpList = (list !! idx)

  chunkUp :: Show a => I -> [a] -> [[a]]
  chunkUp _ [] = []
  chunkUp count ns =
    let (os, ps) = splitAt count ns
      in os : chunkUp count ps

  dropAt :: [a] -> I -> [a]
  dropAt list idx = take idx list ++ drop (idx + 1) list

  li :: [a] -> N
  li str = (length str) - 1

  form :: (Read a, Read b, Show a, Show b) => Len -> C -> [(a, b)] -> String -> String
  form len flag [] accum = accum
  form len flag pairs accum = formed
    where coord = show (fst (head pairs))
          num = show (snd (head pairs))
          out = concat [accum, coord, " --> ", num, (xsp (length num)) ,"  ", (fst $ es flag) ]
          cut = drop 1 pairs
          formed = form len (snd $ es flag) cut out
          es 1   = ("\n", brfl)
          es cnt = ("  ", (cnt - 1))
          xsp sze = take (((len+1) - (sze)) + len) [' ',' '..]

  makeIntoList :: [(a, [N])] -> I -> [[N]] -> [[N]]
  makeIntoList coordVals idx accum
    | (li coordVals) == idx = accum ++ vals
    | otherwise = makeIntoList coordVals (idx + 1) (accum ++ vals)
    where
      vals = [snd (coordVals !! idx)]

  findVals :: String -> Size -> [[N]]
  findVals list cageSize
    | (last list) == '-' = doSub list cageSize
    | (last list) == '+' = doAdd list cageSize
    | otherwise = [[(digitToInt (last list))]]


  rowColClump :: (Pos, [[N]]) -> ((N,N), [[N]])
  rowColClump clump = ((irow, numCol), (snd clump))
    where point = (fst clump)
          ir = digitToInt (head point)
          irow = (ir - 1)
          numCol = (basicIndexOf (last point) colNames 0)

  fixedGrid :: [[[N]]] -> Size -> [[N]]
  fixedGrid values size = chunkUp size (concat (concat (solidify values)))
    where
    solidify :: [[[N]]] -> [[[N]]]
    solidify [] = []
    solidify grid = [toAppend] ++ (solidify (tail grid))
      where this = head grid
            single = head this  
            toAppend = checkingLine this
            checkingLine :: [[N]] -> [[N]] 
            checkingLine [] = []
            checkingLine line = [newLine] ++ (checkingLine (tail line))
              where thisLine = head line
                    newLine = eachPossible thisLine
                    eachPossible :: [N] -> [N]
                    eachPossible [] = []
                    eachPossible chunk 
                      | ((length chunk) == 1) = chunk
                      | otherwise = [0]

  csep :: String -> [String] -> String
  csep sep [] = []
  csep sep arr = (head arr) ++ sep ++ csp (tail arr)
  csp :: [String] -> String
  csp [] = []
  csp arr = (head arr) ++ " " ++ csp (tail arr)
  cnl :: [String] -> String
  cnl [] = []
  cnl arr = (head arr) ++ "\n" ++ cnl (tail arr)
  sa :: Show a => [a] -> [String]
  sa arr = map show arr 
  pl :: [String] -> IO ()
  pl arr = putStrLn (csp arr)
  prl :: Show a => [a] -> IO ()
  prl arr = pl (sa arr)
  pnl :: [String] -> IO ()
  pnl arr = putStrLn (cnl arr)
  prnl :: Show a => [a] -> IO ()
  prnl arr = pnl (sa arr)
  shl :: [a] -> String
  shl arr = show (length arr)
  pls :: String -> [String] -> IO ()
  pls sep arr = putStrLn (csep sep arr)