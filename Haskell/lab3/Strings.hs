-- Maggie Horton
-- CS-231 Winter 2020
-- Haskell Lab 3


module Main where


import Data.List
import Data.Function
import System.Environment
import System.IO
import Data.Char

exs = "------------------"
ls = ("\n" ++ exs ++ exs ++ exs ++ exs++ exs++ exs)
dummyPair = [('0',0)]
-- bee.txt 47 words, 6 lines & 237 chars
-- num.txt: 55 words, 10 lines & 255 chars

main = do
    fName <- getArgs
    fContent <- readFile (fName !! 0)
    let fLines = lines fContent
    let fLength = length fLines
    let totalWordCount = length (words fContent)
    let revLines = reverse fLines
    putStrLn ls
    printWithLineCount revLines  fLength 0
    let twcStr = "\nTotal word count in file: " ++ (show totalWordCount) ++ "\n\n"
    putStrLn twcStr
    
    
printWithLineCount revLineOrd count idx = do
    if count == 0
        then return()
        else do
            let regWordOrd = words (revLineOrd!!idx)
            let revOrd = unwords (reverse regWordOrd)
            let alphOnly = setupLine (unwords regWordOrd)
            let pair = drop 1 (charSurvey (length alphOnly) alphOnly dummyPair)
            let formatted = formatPairs (length pair) "" pair
            let outPut = "LINE " ++ (show count) ++
                    "\n  Original: " ++ (unwords regWordOrd) ++
                    "\n  Reversed: " ++ revOrd ++
                    "\n  Word count: " ++ (show (length regWordOrd)) ++
                    "\n  Occurance of each character displayed as (letter, times):\n\t" ++ formatted ++
                    ls
            putStrLn outPut
            printWithLineCount revLineOrd (count - 1) (idx + 1)

setupLine :: String -> String
setupLine toStrip = stripped 
    where
        lower = map toLower toStrip
        stripped = sort (filter isLetter lower)  -- keep only alphabetic chars

singleCharSurvey :: String -> [(Char,Int)] 
singleCharSurvey "" = []
singleCharSurvey str = pairs
    where
        chopped = filter (/= (str!!0)) str
        diff = (length str) - (length chopped)
        pairs = [((str!!0), diff)]

charSurvey :: Int -> [Char] -> [(Char, Int)] -> [(Char, Int)]
charSurvey 0 str pairs = pairs
charSurvey len str pairs = pairList
    where
        short = filter (/= (str!!0)) str
        updated = pairs ++ (singleCharSurvey str)
        pairList = charSurvey (length short) short updated

formatPairs :: Int -> [Char] -> [(Char, Int)] -> [Char]
formatPairs 0 strn pairList = strn
formatPairs len strn pairList = formatted
    where
        fElem = pairList!!0
        end = strEnd len 
        out = strn ++ "(" ++ [fst fElem] ++ "," ++ (show(snd fElem)) ++ end
        shorter = drop 1 pairList 
        formatted = formatPairs (length shorter) out shorter

strEnd :: Int -> String
strEnd 1 = ")"
strEnd len = "), "