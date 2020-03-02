-- Maggie Horton
-- CS-231
-- Haskell Lab 2
-- SawBoards.hs: converts a String of integers to a list of Int,
-- processing to get a result of list of pairs (Int, Int)

 module SawBoards where
 -- select the smallest length value in each line. smallest = selected
 -- for all remaining boards, saw off length equal to selected board
 

 import Data.List
 import Data.Maybe
 
 
 sawBoards :: String -> [(Int, Int)]
 sawBoards inStr = out
        where
            inList = map read (words inStr) :: [Int] -- read in input & put into list
            len = getLen inList
            num = getNum inList
            out = zip len num

 getLen :: [Int] -> [Int]
 getLen [] = []
 getLen list = out
    where
    out = [minimum list] ++ getLen (math list)
 
 getNum :: [Int] -> [Int]
 getNum [] = []
 getNum list = out
    where
    out = [length list] ++ getNum (math list)
    
 math :: [Int] -> [Int]
 math [] = []
 math list =
    sawed
    where
        sel = minimum list
        newList = filter (/= sel) list
        removedCnt = (length list) - (length newList) -- keep track of number of items removed
        sawed = map (subtract sel) newList
        

    
 

        
        