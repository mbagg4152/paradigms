module Misc where

import Data.Char
import Data.Function
import Data.List
import Data.Ord
import System.Environment

brFlag :: Int
brFlag = 1 

colNames :: [Char]
colNames = "abcdef" 

possiblePairs :: [ColRow] -> [[GData]] -> Target -> IO [(ColRow, GData)]
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

outerPairHelper :: [[ColRow]] -> [[GData]] -> [(ColRow, GData)] -> Index -> IO [(ColRow, GData)]
outerPairHelper str vals pairs idx = do
    let len = length vals
    if len == idx
        then return pairs 
        else do
            pairOne <- innerPairHelper str (vals !! idx) [] 0
            outerPairHelper str vals (pairs ++ pairOne) (idx + 1)

innerPairHelper :: [[ColRow]] -> [GData] -> [(ColRow, Int)] -> Index -> IO [(ColRow, Int)]
innerPairHelper str vals pairs idx = do
    let len = length str
    if len == idx
        then return (pairs)
        else do 
            innerPairHelper str vals (pairs ++ (zip (str !! idx) vals)) (idx + 1)

doSub :: String -> Size -> [[Int]]
doSub chars size 
    | cage > 3 = subHelper (getCageGoal chars) setSm []
    | otherwise = subHelper (getCageGoal chars) sets [] 
    where 
        cage = (digitToInt ((filter isDigit chars) !! 0))
        sets = findSubsets size cage
        setSm = findSubsets size (cage - 1)
       
 
subHelper :: Target -> [[Int]] -> [[GData]] -> [[GData]]
subHelper target [] accum = accum
subHelper target sets accum = subHelper target (delAt sets 0) updated
    where updated = accum ++ (checkPerms target (permutations (sets !! 0)) [] )

checkPerms :: Target -> [[Int]] -> [[GData]] -> [[GData]]
checkPerms target [] valid = valid
checkPerms target perms valid
    | total == target = checkPerms target lessPerms (valid ++ [cur])
    | total /= target = checkPerms target lessPerms valid
    | otherwise = checkPerms target lessPerms valid
    where
        cur = perms !! 0    
        lessPerms = filter (/= cur) perms
        total =  abs (foldl (-) (cur !! 0) (delAt cur 0))  

doAdd :: String -> Size -> [[GData]]
doAdd chars size = addHelper sets [] (getCageGoal chars) 0
    where
        cage = (digitToInt ((filter isDigit chars) !! 0))
        sets = findSubsets size (cage)

addHelper :: [[Int]] -> [[GData]] -> Target -> Index -> [[GData]]
addHelper [] accum target idx  = accum
addHelper list accum target idx 
    | (li list) == idx = accum
    | summed == target = addHelper list (accum ++ [(list !! idx)]) target (idx + 1)
    | summed /= target = addHelper list accum target (idx + 1)
    where summed = sum (list !! idx)
      
findSubsets :: Size -> Size -> [[GData]]
findSubsets maxx size = sort (uniqLists subsets 0)
    where
        legal = repConcat (getLegalVals maxx []) size
        subsets = setGen size legal

indexOf :: Eq a => a -> [a] -> Index -> Index
indexOf lmnt arr idx 
    | lmnt == (arr !! idx) = idx
    | lmnt /= (arr !! idx) = indexOf lmnt arr (idx + 1)
    | otherwise = -1

getCageGoal :: Inputln -> Target
getCageGoal str 
    | (read (wList !! 0)) == 1 = read (wList !! (li wList))
    | str == "" = 0
    | otherwise = target
    where
        wList = words str    
        rmOp = delAt wList (li wList)
        target = read (rmOp !! (li rmOp)) :: Int

getLegalVals :: Size -> [Int] -> [Int]
getLegalVals 0 accum = accum
getLegalVals size accum = sort ([size] ++ (getLegalVals (size -1) accum))

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
repConcat list times = sort (concat (replicate times list))

uniqLists :: Ord a => Eq a => [[a]] -> Index -> [[a]]
uniqLists [] idx = []
uniqLists list idx
    | idx == (li list) = list
    | idx /= (li list) = uniqueLL
    | otherwise = []
    where
        tmpList = sort (list !! idx)
        shortened = filter (/= tmpList) list
        uniqueLL = uniqLists ([tmpList] ++ shortened) (idx + 1)

groupValues ::  [(ColRow, GData)] -> [(ColRow, [GData])]
groupValues = map (\list -> (fst.head $ list, map snd list)) 
              . groupBy ((==) `on` fst)
              . sortBy (comparing fst)

chunkUp :: Index -> [a] -> [[a]]
chunkUp _ [] = []
chunkUp n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunkUp n zs


delAt :: [a] -> Index -> [a]
delAt list idx = take idx list ++ drop (idx + 1) list

li :: [a] -> Int
li str = (length str) - 1

form :: (Read a, Read b, Show a, Show b) => Len -> Count -> [(a, b)] -> String -> String
form 0 nlf pairs accum = accum
form len nlf pairs accum = formed
    where 
        f = pairs !! 0
        coord = show (fst f)
        num = show (snd f)
        es = fst (endsep nlf)
        nnlf = snd (endsep nlf)
        out = accum ++ "" ++ coord ++ " --> " ++ num ++ " " ++ es
        cut = drop 1 pairs
        formed = form (length cut) nnlf cut out

endsep :: Count -> (String, Int)
endsep 1 = ("\n" , brFlag)
endsep cnt = ("", (cnt - 1))



type Count = Int
type Col = Int
type Index = Int
type GData = Int
type Len = Int
type Row = Int
type Size = Int
type Target = Int

type ColRow = String
type Inputln = String