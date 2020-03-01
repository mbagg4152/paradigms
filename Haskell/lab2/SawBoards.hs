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
 sawBoards inStr =
    [(length, number)] where
    inList = map read (words inStr) :: [Int] -- read in input & put into list
    selected = minimum inList -- select the smallest length
    out = process inList
    length = fst out -- length is the first element of the outputted tuple
    number = snd out -- number is the second element of outputted tuple
 
 
 process :: [Int] -> (Int,Int)
 process list =
    (len, num) where
    sel = minimum list
    indx = fromMaybe 0 (elemIndex sel list) -- convert index from Maybe Int to Int. indx becomes 0 if elemIndex returns Nothing
    len = sel
    num = indx
    
 deleteAt :: [Int] -> Int -> [Int]
 deleteAt list index =
    [elements] where
    split = splitAt index list -- value to delete becomes first value of second list
    l1 = fst split
    l2 = drop 1 (snd split) -- remove first element of list
    
    
    
    
    
    
    [elements] = list
    
    