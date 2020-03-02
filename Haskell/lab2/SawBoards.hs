-- Maggie Horton
-- CS-231
-- Haskell Lab 2
-- SawBoards.hs: converts a String of integers to a list of Int,
-- processing to get a result of list of pairs (Int, Int)

 module SawBoards where
 import Data.List
 import Data.Maybe
 
 -- this is the core function of this file/module.
 -- takes in a string, which is read in from a file &
 -- 'saws' and 'stacks' the boards and outputs the results
 -- as a list of tuples
 sawBoards :: String -> [(Int, Int)]
 sawBoards inStr = out
        where
            inList = map read (words inStr) :: [Int] -- read in input & put into list
            len = getLen inList -- get a list of 'sizes' after doing the 'sawing'
            num = getNum inList -- get a list of occurrences of each 'size' after doing the 'sawing'
            out = zip len num -- create list of pairs/tuples from the lengths & numbers

 
 -- returns a list of lengths after completing the 'sawing'
 -- process
 getLen :: [Int] -> [Int]
 getLen [] = []
 getLen list = out
    where
    out = [minimum list] ++ getLen (updateList list)
 
  -- returns a list of numbers after completing the 'sawing'
  -- process
 getNum :: [Int] -> [Int]
 getNum [] = []
 getNum list = out
    where
    out = [length list] ++ getNum (updateList list)
 
 
 -- takes in a list, finds the minimum value of the list, removes it,
 -- subtracts said value from each number in the list & then returns
 -- the list. this is the 'sawing' process
 updateList :: [Int] -> [Int]
 updateList [] = []
 updateList list = sawed
    where
        sel = minimum list -- smallest val in list
        newList = filter (/= sel) list -- make new list after removing all instances of the smallest val
        removedCnt = (length list) - (length newList) -- keep track of number of items removed
        sawed = map (subtract sel) newList -- subtract smallest val from each val in list
        

    
 

        
        