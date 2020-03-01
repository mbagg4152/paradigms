 -- Not part of assigment, for testing code sections only
 
 module Main where
 

 import Data.List
 import Data.Maybe
 
 main = do
  let list = [3,77,8,23,2,6,8,80]
  let min = minimum list
  print "min " ++ show min
  let idx = fromMaybe 0 (elemIndex min list)
  let split = splitAt idx list
  let sec = drop 1 (snd split)
  print sec