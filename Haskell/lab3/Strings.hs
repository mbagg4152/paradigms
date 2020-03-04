-- Maggie Horton
-- CS-231 Winter 2020
-- Haskell Lab 3


module Main where

import Data.Set
import Data.List
import Data.Function
import System.Environment
import System.IO

-- note: bee.txt has 47 words, 6 lines & 237 chars

main = do
    fName <- getArgs
    fContent <- readFile (fName !! 0)
    let fLines = lines fContent
    putStrLn (show fName)
    let len = length fLines
    let wordCount = length (words fContent)
    putStrLn (show len)
    putStrLn (show wordCount)
    lastLineFirst fLines
    
    
lastLineFirst fLines = do
    let rev = reverse fLines
    mapM_ print rev


--wordsPerLine :: [String] -> Int



--countChars :: [String] -> [(Char,Int)]

-- getCharList :: [(String,[Int])] ->[(String,[Int])]
-- getCharList [] = []
-- getCharList tup = list
--     where
        
    


pr val = do
    putStrLn val



--getEachCharCount :: [String] -> [Int]

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList