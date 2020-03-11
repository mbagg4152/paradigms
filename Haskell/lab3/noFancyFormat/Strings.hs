module Main where

import Data.Char
import Data.List
import System.Environment

main = do
    names <- getArgs
    text <- readFile (names!!0)

    let outFName = names!!1 
    let fLines = lines text 
    let wc = length (words text) 
    let rFLines = reverse fLines
    let letters = getUpperOnly (unlines fLines)
    let letterLen = length letters 
    
    let dummy = [('+',999)]
    let fileFreq = drop 1 (letterAppearance letterLen letters dummy )
    let formFreq = outputFormatter (length fileFreq) "" fileFreq 
    
    writeFile outFName ""
    processAppend rFLines (length fLines) 0 outFName
    appendFile outFName ("\n\nFor whole file\n" ++  "Word count: " ++ (show wc) ++ "\n"++ formFreq ++ "\n")

getUpperOnly :: String -> String
getUpperOnly toStrip = upperChars
    where
        up = map toUpper toStrip
        lettersOnly = filter isLetter up
        upperChars = sort lettersOnly      

letterAppearance :: Int -> [Char] -> [(Char, Int)] -> [(Char, Int)]
letterAppearance 0 inLine tupList = tupList
letterAppearance len inLine tupList = freqPair
    where
        fChar = inLine!!0
        less = filter (/= fChar) inLine
        lessLen = length less
        upTupList = tupList ++ (helper inLine)
        freqPair = letterAppearance  lessLen less upTupList
        
        
helper :: String -> [(Char,Int)] 
helper "" = [] 
helper inLine = tupList
    where
        fChar = inLine!!0
        less = filter (/= fChar) inLine
        inLnLen = length inLine
        lessLen = length less
        appearRate = inLnLen - lessLen 
        tupList = [(fChar, appearRate)]
        
              
outputFormatter :: Int -> [Char] -> [(Char, Int)] -> [Char]
outputFormatter 0 freqOutput freqPair = freqOutput
outputFormatter len freqOutput freqPair = appearOutput
    where
        out = freqOutput ++ "\t" ++ [fst (freqPair!!0)] ++ " appears " ++ (show(snd (freqPair!!0))) ++ " time(s)\n"
        shorter = drop 1 freqPair
        sLen = length shorter 
        appearOutput = outputFormatter sLen out shorter 


processAppend rFLines lineCount pos outFName = do
    if lineCount == 0
        then return()
        else do
            let inpWords = words (rFLines!!pos)  
            let reversed = unwords (reverse inpWords)  
            let letters = getUpperOnly (unwords inpWords)
            let letterLen = length letters
            let dummy = [('+',999)]  
            let appearance = drop 1 (letterAppearance  letterLen letters dummy)
            let appearOutput = outputFormatter (length appearance) "" appearance 
            let output = "\nLine no. " ++ (show lineCount) ++ "\nLine input: " ++ (unwords inpWords) ++ "\nReveresed word order: " ++ reversed ++  "\nWord lineCount: " ++ (show (length inpWords)) ++ "\n" ++ appearOutput ++ "\n"
            appendFile outFName output 
            processAppend rFLines (lineCount - 1) (pos + 1) outFName 

  


