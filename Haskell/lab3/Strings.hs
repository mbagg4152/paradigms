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
    let sep = "----------------------------------------------------------------"
    putStrLn sep
    printWithLineCount rev  len 0
    
    
printWithLineCount revLineOrd count idx = do
    if count == 0
        then return()
        else do
            let regWordOrd = words (revLineOrd!!idx)
            let revWordOrd = regWordOrd
            let wCount = length regWordOrd
            let tmp = unwords (reverse revWordOrd)
            
            let lineNum = "LINE "++(show count)
            let orgStr = "\nOriginal: " ++ (unwords regWordOrd)
            let revStr = "\nReversed: "++tmp
            let wcStr = "\nword count: "++(show wCount)
            let sep = "\n----------------------------------------------------------------"
            let out = lineNum++orgStr++revStr++wcStr++sep
            
            putStrLn out
            printWithLineCount revLineOrd (count - 1) (idx + 1)


mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList