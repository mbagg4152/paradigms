-- Maggie Horton
-- CS-231
-- Haskell Lab 2
-- SawBoards.hs: converts a String of integers to a list of Int,
-- processing to get a result of list of pairs (Int, Int)

 module SawBoards
 where

 -- imports


 -- code

 -- select the smallest length value in each line. smallest = selected
 -- for all remaining boards, saw off length equal to selected board
 sawBoards :: String -> [(Int, Int)]
 sawBoards input =
    [(length, number)] where
    length = 0
    number = 1