-- Maggie Horton
-- CS-231 Winter 2020
-- Haskell Lab 3


module Main where


import Data.List
import Data.Function
import System.Environment
import System.IO
import Data.Char

-- bee.txt 47 words, 6 lines & 237 chars
-- num.txt: 55 words, 10 lines & 255 chars

main = do
    fName <- getArgs
    fContent <- readFile (fName !! 0)
    let fLines = lines fContent
    let fLength = length fLines
    let totalWordCount = length (words fContent)
    let revLines = reverse fLines
    let sep = "----------------------------------------------------------------"
    putStrLn sep
    printWithLineCount revLines  fLength 0
    let twcStr = "\nTotal word count in file: "++(show totalWordCount) ++"\n\n"
    putStrLn twcStr
    
    
printWithLineCount revLineOrd count idx = do
    if count == 0
        then return()
        else do
            let regWordOrd = words (revLineOrd!!idx)
            let revWordOrd = regWordOrd
            let wordCount = length regWordOrd
            let revOrd = unwords (reverse revWordOrd)
            
            let lineNum = "LINE " ++ (show count)
            let orgStr = "\nOriginal: " ++ (unwords regWordOrd)
            let revStr = "\nReversed: " ++ revOrd
            let wcStr = "\nword count: " ++ (show wordCount)
            let sep = "\n----------------------------------------------------------------"
            let stripped = "\n"++ stripNonAlpha (unwords regWordOrd)
            let out = lineNum++orgStr++revStr++wcStr++stripped++sep
            putStrLn out
            printWithLineCount revLineOrd (count - 1) (idx + 1)

stripNonAlpha :: String -> String
stripNonAlpha toStrip = stripped 
    where
        lower = map toLower toStrip
        alpha = filter isLetter  lower
        stripped = alpha