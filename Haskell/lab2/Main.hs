module Main where
import SawBoards
import System.Environment
import System.IO
{-
Dr. Vineyard, CS231 Winter 2020
Main program for Haskell lab 2, sawing boards
-}

--Program to calculate board lengths and numbers of each length

--makeOutput concatenates the input line and calculated list into
--a labelled, readable String for output

makeOutput :: String -> [(Int, Int)] -> String
makeOutput input boards = formatInput input ++ formatBoards boards

-- formatBoards formats the result of calling function sawBoards
formatBoards :: [(Int, Int)] -> String
formatBoards boards = "\tCut boards listed as length: number\n\t\t" ++
                      addCutBoards boards

--addCutBoards is the helper function for formatBoards, which actually
--  recursively does all the work.
addCutBoards [] = []
addCutBoards[(len, num)] = (show len) ++ ": " ++ (show num) ++ "\n"
addCutBoards ((len, num):bds) = (show len) ++ ": " ++ (show num) ++
                                 ", " ++ addCutBoards bds

--formatInput
--input is a String consisting of integer values
--create a comma separated string of numbers for output
formatInput :: String -> String
formatInput s = "\tBoards listed by length\n\t\t" ++ addBoards (words s)

--addBoards is the helper function for formatInput, which
--  does all the work recursively
addBoards :: [String] -> String
addBoards [] = "\n"
addBoards [x] = x ++ "\n"
addBoards (x:xs) = x ++ ", " ++ addBoards xs

--processInput is a recursive function to read each line of
--  the input file to use as an argument to sawBoards
--  the result of sawBoards is passed to makeOutput to produce
--  a String to write to the output file

processInput inFileHandle outFileHandle = do
  eof <- hIsEOF inFileHandle
  if eof
    then return ()
    else do
        inputLine <- hGetLine inFileHandle
        let boardList = sawBoards inputLine
        -- sawBoards has declaration
        -- sawBoards :: String -> [(Int, Int)]
        let outputLines = makeOutput inputLine boardList
        hPutStrLn outFileHandle outputLines
        processInput inFileHandle outFileHandle
{-
main is the entry point to the program.  Compile the code with command
     ghc --make Main.hs
then run the program with command
     ./Main input output
where input is the name of the input file and output is the name of the
output file.
-}

main = do
  [inFile, outFile] <- getArgs
  inFileHandle <- openFile inFile ReadMode
  outFileHandle <- openFile outFile WriteMode
  hPutStrLn outFileHandle "               Sawing Boards"
  hPutStrLn outFileHandle ""
  processInput inFileHandle outFileHandle
  hClose inFileHandle
  hClose outFileHandle
  