-- Maggie Horton
-- CS-231 Winter 2020
-- Haskell Lab 3


module Main where

import Data.Char
import Data.Function
import Data.List
import System.Environment
import System.IO

sep = "\n\n" 
dummyPair = [('0',0)]

main = do
    fNames <- getArgs
    fContent <- readFile (fNames!!0)
    let outName = fNames!!1
    let fLines = lines fContent
    let totalWordCount = length (words fContent)
    let revLines = reverse fLines
    writeFile outName ("\nTotal word count in file: " ++ (show totalWordCount) ++ "\n\n")
    let allAlpha = setupLine (unlines fLines)
    let totalFreq = drop 1 (charSurvey (length allAlpha) allAlpha dummyPair)
    let formattedFreq = formatPairs (length totalFreq) "" totalFreq
    appendFile outName ("Frequency of each letter in file displayed as (letter, times):\n\t" ++ formattedFreq ++ sep) 
    appendWithLineCount revLines (length fLines) 0 outName
    
appendWithLineCount revLines count idx foName = do
    if count == 0
        then return()
        else do
            let origWords = words (revLines!!idx)  -- line order fed in reverse, but word order still the same per line
            let revWords = unwords (reverse origWords)  -- reverse word order in line
            let sortedAlpha = setupLine (unwords origWords)  -- stripped non-alpha chars & sorted
            let charFreq = drop 1 (charSurvey (length sortedAlpha) sortedAlpha dummyPair)
            let formatted = formatPairs (length charFreq) "" charFreq
            let outPut = "LINE " ++ (show count) ++ 
                         "\n  Original: " ++ (unwords origWords) ++
                         "\n  Reversed: " ++ revWords ++ 
                         "\n  Word count: " ++ (show (length origWords)) ++
                         "\n  Frequency of each letter in line displayed as (letter, times):\n\t" ++ formatted ++ sep
            appendFile foName outPut
            appendWithLineCount revLines (count - 1) (idx + 1) foName

setupLine :: String -> String
setupLine toStrip = sort (filter isLetter (map toLower toStrip))  -- keep only alphabetic chars 
    --where
      --  lower = map toLower toStrip
        --stripped = 

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