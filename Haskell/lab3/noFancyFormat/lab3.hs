-- Brendan Gallagher
-- Haskell Lab 3

module Main where

import Data.Char
import Data.List
import System.Environment

-- values used for separating output in file
hln = "#####################"
lnDiv = "\n\n"++hln++hln++hln++hln++"\n"

-- Main function of file. This function does the following: 
-- read file, output lines backwards, output each original line
-- output lines with words in reversed order and output appRt
-- rate of each letter 
main = do
    names <- getArgs
    inputText <- readFile (names!!0)
    let outText = names!!1
    let inpLines = lines inputText

    -- file word count
    let wc = length (words inputText) 
    
    -- set up so lines print in order last -> first
    let revFLines = reverse inpLines  
    
    -- list of all alphabetic chars in upper case
    let letters = getUpperOnly (unlines inpLines) 
    let letterLen = length letters 
    
    -- get letter appearance rate for entire file
    let totalAppRate = letterAppearRate letterLen letters []
    
    -- formatted appearance rate
    let formattedApp = outputFormatter (length totalAppRate) "" totalAppRate 
    
    writeFile outText "" -- delete old contents
    processAppend revFLines (length inpLines)  0 outText

    -- strings to be added to EOF
    let foHead = "\nEntire file\n"
    let wcStr = "\tFile word count = " ++ (show wc)
    let appearStr = "\n\tLetter appearances in the entire file:\n\t" ++ 
                    formattedApp
    
    appendFile outText (foHead ++ wcStr  ++ appearStr ++ lnDiv)

-- This function is used for easier processing
    -- Line string -> Uppercase alphabetic chars only
getUpperOnly :: String -> String 
getUpperOnly toStrip = upperChars
    where
        up = map toUpper toStrip
        lettersOnly = filter isLetter up
        upperChars = sort lettersOnly      

-- This function gets appearance rates of each letter
    -- List length -> File line string -> List of appearance rates, [(rate,letter)] -> 
    -- Updated list of appearance rates, [(rate,letter)]
letterAppearRate :: Int -> String -> [(Int, Char)] -> [(Int, Char)]
letterAppearRate 0 inLine tupList = tupList
letterAppearRate size inLine [] = pairs
    where
        first = inLine!!0
        less = filter (/= first) inLine
        lessLen = length less
        upTupList = helper inLine
        pairs = letterAppearRate  lessLen less upTupList
letterAppearRate size inLine tupList = pairs
    where
        first = inLine!!0
        less = filter (/= first) inLine
        lessLen = length less
        upTupList = tupList ++ (helper inLine)
        pairs = letterAppearRate  lessLen less upTupList
        

-- Helper for the letterAppearRate function
    -- Line string -> List of appearance rate of first letter in list [(rate,letter)]
helper :: String -> [(Int, Char)] 
helper "" = [] 
helper inLine = tupList
    where
        first = inLine!!0
        less = filter (/= first) inLine
        inLnLen = length inLine
        lessLen = length less
        appearRate = inLnLen - lessLen 
        tupList = [(appearRate, first)]


-- Function for formatting occurance rate
    -- Current line count -> Either new line or a blank formatting function
lineBreak :: Int -> String
lineBreak count
    | lb == 0 = "\n\t"
    | otherwise = ""
    where lb = mod count 3


-- Format appearance rate output
    -- List length -> Currently formatted string -> Letter apperance rate 
    -- [(rate,letter)] -> Updated formatted string
outputFormatter :: Int -> String -> [(Int,Char)] -> String
outputFormatter 0 formStr pairs = formStr
outputFormatter size formStr pairs = appearOutput
    where
        brk = lineBreak size 
        out = formStr ++ "\t" ++ [snd (pairs!!0)] ++ " appears " ++ 
              (show(fst (pairs!!0))) ++ " time(s)." ++ brk
        shorter = drop 1 pairs
        sLen = length shorter 
        appearOutput = outputFormatter sLen out shorter 


-- Append output to file
processAppend revFLines lineCount pos outText = do
    if lineCount == 0
        then return()
        else do
            let inpWords = words (revFLines!!pos)  
            let reversed = unwords (reverse inpWords)  
            let letters = getUpperOnly (unwords inpWords)
            let letterLen = length letters
            let appRt = letterAppearRate letterLen letters []
            let appearOutput = outputFormatter (length appRt) "" appRt

            -- strings to be written to file
            let lnNo = "\nLine no. " ++ (show lineCount)
            let actInpStr = "\n\tOriginal line input:\n\t\t" ++ (unwords inpWords)
            let revInpStr = "\n\tReveresed word order:\n\t\t" ++ reversed
            let wcStr = "\n\tWord count for line = " ++ (show (length inpWords))
            let appRtStr = "\n\tLetter appearances in line:\n\t" ++ appearOutput
            let output = lnNo ++ actInpStr ++ revInpStr ++ wcStr ++ appRtStr ++ lnDiv
            
            appendFile outText output 
            processAppend revFLines (lineCount - 1) (pos + 1) outText 


  


