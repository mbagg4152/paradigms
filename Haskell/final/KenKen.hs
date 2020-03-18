module Main where

import Data.Char
import Data.Function
import Data.List
import Data.Ord
import System.Environment

brFlag = 1 -- flag for inserting newlines
colNames = "abcdef"

main = do
    fName <- getArgs
    fcontent <- readFile (fName !! 0)
    let fLines = lines fcontent
    let dimens = read (fLines !! 0) :: Int
    putStrLn ("Board size: " ++ (show dimens))
    let noDimen = delAt fLines 0
    coords <- process noDimen 0 (length noDimen) dimens []
    let sc = sort coords
    let groups = groupValues coords
    let coordstr = form (length sc) brFlag sc ""
    let groupStr = form (length groups) brFlag groups ""
    putStrLn ("\n\n" ++ groupStr)

groupValues ::  [(String, Int)] -> [(String, [Int])]
groupValues = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst) . sortBy (comparing fst)

findSubsets :: Int -> Int -> [[Int]]
findSubsets lim size = sub
    where
        legal = repConcat (getLegalVals lim []) size
        subsets = setGen size legal
        sub = sort (uniqLists subsets 0)

process :: [String] -> Int -> Int -> Int -> [(String,Int)] -> IO [(String,Int)]
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

possiblePairs :: [String] -> [[Int]] -> Int -> IO [(String,Int)]
possiblePairs str vals target = do
    let size = (read (str !! 0)) :: Int
    let coord = str !! 1
    if size == 1
        then return [(coord, target)]
        else do
            let noSze = delAt str 0
            let noOp = delAt noSze (li noSze)
            let coordsOnly = delAt noOp (li noOp)
            coordPairs <- outerPairHelper (permutations coordsOnly) vals [] 0
            return (coordPairs)

outerPairHelper :: [[String]] -> [[Int]] -> [(String,Int)] -> Int -> IO [(String,Int)]
outerPairHelper str vals pairs idx = do
    let len = length vals
    if len == idx
        then return pairs 
        else do
            pairOne <- innerPairHelper str (vals !! idx) [] 0
            outerPairHelper str vals (pairs ++ pairOne) (idx + 1)

innerPairHelper :: [[String]] -> [Int] -> [(String,Int)] -> Int -> IO [(String,Int)]
innerPairHelper str vals pairs idx = do
    let len = length str
    if len == idx
        then return (pairs)
        else do innerPairHelper str vals (pairs ++ (zip (str !! idx) vals)) (idx + 1)

coordConvert :: String -> String
coordConvert alphaCoord = numCol ++ row
    where    
        col = alphaCoord !! 0
        row = [alphaCoord !! 1]
        numCol = show ((indexOf col colNames 0) + 1)

indexOf :: Eq a => a -> [a] -> Int -> Int
indexOf elem arr idx 
    | elem == (arr !! idx) = idx
    | elem /= (arr !! idx) = indexOf elem arr (idx + 1)
    | otherwise = -1
      
findVals ::  [Char] -> Int -> [[Int]] 
findVals list size
    | op == '-' = doSub list size
    | op == '+' = doAdd list size
    | otherwise = [[(digitToInt op)]]
    where op = list !! (li list)

doSub :: String -> Int -> [[Int]]
doSub chars size = list
    where 
        intList = map digitToInt (filter isDigit chars)
        target = getCageGoal chars
        sets = findSubsets size (intList !! 0) 
        list = subHelper target sets []
 
subHelper :: Int -> [[Int]] -> [[Int]] -> [[Int]]
subHelper target [] accum = accum
subHelper target sets accum = subHelper target (delAt sets 0) updated
    where updated = accum ++ (checkPerms target (permutations (sets !! 0)) [] )

checkPerms :: Int -> [[Int]] -> [[Int]] -> [[Int]]
checkPerms target [] valid = valid
checkPerms target perms valid
    | total == target = checkPerms target lessPerms updated
    | total /= target = checkPerms target lessPerms valid
    | otherwise = checkPerms target lessPerms valid
    where
        cur = perms !! 0    
        lessPerms = filter (/= cur) perms
        updated = valid ++ [cur]
        total =  foldl (-) (cur !! 0) (delAt cur 0)  

doAdd :: [Char] -> Int -> [[Int]]
doAdd chars size = list
    where 
        intList = map digitToInt (filter isDigit chars)
        goal = getCageGoal chars
        sets = findSubsets size (intList!!0)
        list = addHelper sets [] goal 0

addHelper :: [[Int]] -> [[Int]] -> Int -> Int -> [[Int]]
addHelper list accum target idx 
    | (li list) == idx = accum
    | summed == target = addHelper list (accum ++ [(list !! idx)]) target (idx + 1)
    | summed /= target = addHelper list accum target (idx + 1)
    where summed = sum (list !! idx)
      
getCageGoal :: String -> Int
getCageGoal str 
    | (read (wList !! 0)) == 1 = read (wList !! (li wList))
    | str == "" = 0
    | otherwise = target
    where
        wList = words str    
        rmOp = delAt wList (li wList)
        target = read (rmOp !! (li rmOp)) :: Int

getLegalVals :: Int -> [Int] -> [Int]
getLegalVals 0 intList = intList
getLegalVals size intList = sort ([size] ++ (getLegalVals (size -1) intList))

setGen :: Int -> [Int] -> [[Int]]
setGen 0 _ = [[]]
setGen _ [] = []
setGen lim (cur:next) = [cur : subs | subs <- setGen (lim - 1) next] ++ setGen lim next 

repConcat :: [Int] -> Int -> [Int]
repConcat list times = sort (concat (replicate times list))

uniqLists :: Ord a => Eq a => [[a]] -> Int -> [[a]]
uniqLists [] idx = []
uniqLists list idx
    | idx == ((length list)-1) = list
    | idx /= ((length list)-1) = uniqueLol
    | otherwise = []
    where
        tmpList = sort(list !! idx)
        shortened = filter (/= tmpList) list
        newList = [tmpList] ++ shortened
        uniqueLol = uniqLists newList (idx + 1)

delAt :: [a] -> Int -> [a]
delAt list idx = take idx list ++ drop (idx + 1) list

li :: [a] -> Int
li str = (length str) - 1

form :: Show a => Int -> Int -> [(String, a)] -> String -> String
form 0 nlf pairs accum = accum
form len nlf pairs accum = formed
    where 
        f = pairs !! 0
        coord = fst f
        num = (show (snd f))
        es = fst (endsep nlf)
        nnlf = snd (endsep nlf)
        out = accum ++ "(" ++ coord ++ ", " ++ num ++ ") " ++ es
        cut = drop 1 pairs
        formed = form (length cut) nnlf cut out

endsep :: Int -> (String, Int)
endsep 1 = ("\n" , brFlag)
endsep cnt = ("", (cnt - 1))





