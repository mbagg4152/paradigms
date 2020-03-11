-- Maggie Horton
-- CS-231 Winter 2020
-- Haskell Lab 3


module Main where

import Data.Char
import Data.Function
import Data.List
import System.Environment
import System.IO

dnl = "\n\n"
nl = "\n" 
pfxPair = "\n\t  "
pfxFLine ="\n  "
initPair = [('0',0)]
brFlag = 5
wCntLbl = "Word count: "
freqLbl = "Letter frequency as (letter, times):"
em = ""

main = do
    fNames <- getArgs
    fContent <- readFile (fNames!!0)
    let outName = fNames!!1
    let fLines = lines fContent
    let totalWordCount = length (words fContent)
    let revLines = reverse fLines
    let allAlpha = setupLine (unlines fLines)
    let totalFreq = drop 1 (charSurvey (length allAlpha) allAlpha initPair)
    let formattedFreq = formatPairs (length totalFreq) em totalFreq brFlag
    writeFile outName ("FOR ENTIRE FILE\n" ++ 
                        wCntLbl ++ (show totalWordCount) ++ 
                        nl ++ freqLbl ++ pfxPair ++ formattedFreq ++ 
                        dnl)
    appendWithLineCount revLines (length fLines) 0 outName
    
appendWithLineCount revLines count idx foName = do
    if count == 0
        then return()
        else do
            let origWords = words (revLines!!idx)  -- line order fed in reverse, but word order still the same per line
            let revWords = unwords (reverse origWords)  -- reverse word order in line
            let sortedAlpha = setupLine (unwords origWords)  -- stripped non-alpha chars & sorted
            let charFreq = drop 1 (charSurvey (length sortedAlpha) sortedAlpha initPair)
            let formatted = formatPairs (length charFreq) em charFreq brFlag
            let outPut = nl ++ "LINE " ++ (show count) ++ 
                         pfxFLine  ++ "Original: " ++ (unwords origWords) ++
                         pfxFLine  ++ "Reversed: " ++ revWords ++ 
                         pfxFLine  ++ wCntLbl ++ (show (length origWords)) ++
                         pfxFLine  ++ freqLbl ++ pfxPair ++ formatted ++ dnl
            appendFile foName outPut
            appendWithLineCount revLines (count - 1) (idx + 1) foName

-- keep only alphabetic chars & convert all to lowercase
setupLine :: String -> String
setupLine toStrip = sort (filter isLetter (map toLower toStrip))   

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

formatPairs :: Int -> [Char] -> [(Char, Int)] -> Int -> [Char]
formatPairs 0 strn pairList nlFlag = strn
formatPairs len strn pairList nlFlag = formatted
    where
        fElem = pairList!!0
        endParen = strEnd len
        endSep = fst (optEnd nlFlag)
        nFlag = snd (optEnd nlFlag) 
        out = strn ++ "(" ++ [fst fElem] ++ "," ++ (show(snd fElem)) ++ endParen ++ endSep
        shorter = drop 1 pairList 
        formatted = formatPairs (length shorter) out shorter nFlag

strEnd :: Int -> String
strEnd 1 = ")"
strEnd len = "), "

optEnd :: Int -> (String, Int)
optEnd 0 = ("\n    " , brFlag)
optEnd count = ("", (count-1))