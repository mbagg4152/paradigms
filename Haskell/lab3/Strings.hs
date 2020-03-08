-- Maggie Horton
-- CS-231 Winter 2020
-- Haskell Lab 3


module Main where

import Data.Set
import Data.List
import Data.Function
import System.Environment
import System.IO

-- bee.txt 47 words, 6 lines & 237 chars
-- num.txt: 55 words, 10 lines & 255 chars

main = do
    fName <- getArgs
    fContent <- readFile (fName !! 0)
    let fLines = lines fContent
    let len = length fLines
    let wordCount = length (words fContent)
    let rev = reverse fLines
    let wCount = 0
    printWithLineCount rev len 0 wCount
    
    
printWithLineCount line count idx wc = do
    if count == 0
        then return()
        else do
            let tmpWords = words (line!!idx)
            let tmp = unwords (reverse tmpWords)
            let out = (show count) ++ ". "++ tmp
            putStrLn out
            printWithLineCount line (count - 1) (idx + 1) 
        
        



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