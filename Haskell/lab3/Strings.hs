-- Maggie Horton
-- CS-231 Winter 2020
-- Haskell Lab 3 - Strings.hs
-- For each line of input file, program will:
    -- Print the line number (last line first)  
    -- Print the line which was input. 
    -- Print the line with the words in reverse order. 
    -- Print the number of words for this line.
    -- Print the count of each alphabetical character for the line.
-- After, the program prints total number of words and frequency of 
-- each alphabetic character in the entire file

module Main where

import Data.Char
import Data.Function
import Data.List
import System.Environment
import System.IO

initPair = [('0',0)] -- temp value for starting to find char frequencies

-- formatting/output global values
brFlag = 7 -- flag for inserting newlines
es = "" -- empty string
lblFreq = "Letter frequency displayed as (character, frequency):" -- char frequency label
lblWc = "Word count: " -- word count label
nld = "\n\n" -- double new line
nls = "\n" -- single new line
pPair = "\n\t  " -- put before frequency pairs
pLine ="\n  " -- put before line data/output

-- main function of the file which reads in the contents of a file,
-- processes it, then writes the required output into a second file.
-- input & output file names are specified in the command line
main = do
    fNames <- getArgs
    fContent <- readFile (fNames!!0)
    let outName = fNames!!1 -- name of file to write to
    let fLines = lines fContent -- get content as string list
    let totalWordCount = length (words fContent) -- get file word count
    let revLines = reverse fLines -- reverse line order
    let allAlpha = stripAndSort (unlines fLines) -- setup file contents for processing
    let fileFreq = drop 1 (getCharFreq (length allAlpha) allAlpha initPair) -- get frequencies of letters & remove initial filler pair
    let formFreq = formatFreq (length fileFreq) es fileFreq brFlag -- formatted letter frequency
    
    writeFile outName es -- clear past file contents
    appendWithLineCount revLines (length fLines) 0 outName
    appendFile outName (nld ++ "FOR ENTIRE FILE\n" ++ 
                        lblWc ++ (show totalWordCount) ++ 
                        nls ++ lblFreq ++ pPair ++ formFreq ++ 
                        nld)
    

-- this function finds the required data and then
-- appends said data to the end of the file
-- data includes: original line, line with reversed word
-- order, word count, line number & frequency of each
-- letter in the line
appendWithLineCount revLines count idx foName = do
    if count == 0
        then return()
        else do
            let origWords = words (revLines!!idx)  -- line order fed in reverse, but word order still the same per line
            let revWords = unwords (reverse origWords)  -- reverse word order in line
            let sortedAlpha = stripAndSort (unwords origWords)  -- stripped non-alpha chars & sorted
            let charFreq = drop 1 (getCharFreq (length sortedAlpha) sortedAlpha initPair)
            let formatted = formatFreq (length charFreq) es charFreq brFlag -- formatted letter frequency
            let fOutput = nls ++ "LINE " ++ (show count) ++ 
                         pLine  ++ "Original: " ++ (unwords origWords) ++
                         pLine  ++ "Reversed: " ++ revWords ++ 
                         pLine  ++ lblWc ++ (show (length origWords)) ++
                         pLine  ++ lblFreq ++ pPair ++ formatted ++ nld
            appendFile foName fOutput -- append contents to the end of the output file
            appendWithLineCount revLines (count - 1) (idx + 1) foName -- append to file until no lines left

-- keep only alphabetic chars & convert all to lowercase
-- used in the process of outputting letter frequencies
stripAndSort :: String -> String
stripAndSort toStrip = sort (filter isLetter (map toLower toStrip))   


-- finds the frequency of each letter per line or file
-- sends back a list of tuples, (letter, frequency)
getCharFreq :: Int -> [Char] -> [(Char, Int)] -> [(Char, Int)]
getCharFreq 0 line pairs = pairs -- base case
getCharFreq len line pairs = freqPair
    where
        chopped = filter (/= (line!!0)) line
        updatedPairs = pairs ++ (freqHelper line)
        freqPair = getCharFreq (length chopped) chopped updatedPairs


-- helper function used in finding the frequencies of
-- each letter
freqHelper :: String -> [(Char,Int)] 
freqHelper "" = [] -- base case
freqHelper line = pairs
    where
        chopped = filter (/= (line!!0)) line
        diff = (length line) - (length chopped)
        pairs = [((line!!0), diff)]

        
-- used to format letter frequencies so that the frequencies
-- aren't just output as a list of tuples        
formatFreq :: Int -> [Char] -> [(Char, Int)] -> Int -> [Char]
formatFreq 0 freqOutput freqPair nlFlag = freqOutput
formatFreq len freqOutput freqPair nlFlag = formatted
    where
        fElem = freqPair!!0 -- frequency pair
        char = [fst fElem] -- letter
        freq = (show(snd fElem)) -- times letter appears
        eParen = getEndParen len -- closing parethesis, either with or without trailing comma
        eSep = fst (getEndSep nlFlag) -- end line separator, either \n or blank
        upFlag = snd (getEndSep nlFlag) -- updated newline flag val
        out = freqOutput ++ "(" ++ char ++ "," ++ freq ++ eParen ++ eSep -- formatted output
        shorter = drop 1 freqPair -- remove first element in list 
        sLen = length shorter 
        formatted = formatFreq sLen out shorter upFlag -- keep formatting until list is empty


-- determines whether or not to put a comma after a frequency pair
getEndParen :: Int -> String
getEndParen 1 = ")"
getEndParen len = "), "

-- determines if a newline needs to be placed after frequency pair
getEndSep :: Int -> (String, Int)
getEndSep 0 = ("\n    " , brFlag)
getEndSep count = ("", (count-1))