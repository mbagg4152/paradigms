module Main where
import MakeSquares
import System.Environment
import System.IO

--Program to divide a rectangle into squares.
--This module handles all I/O and calls solve in module MakeSquares
--to actually change an input String from a file into a list of
--Int values representing the squares.

--makeOutput concatenates the input line and output list into
--a labelled, readable String for output

makeOutput inputNums outputList =
           (inputPart inputNums) ++ " " ++ (outputPart outputList)

--inputPart is a helper function for makeOutput which makes input
--line from file readable

inputPart nums = "length: " ++ (head numList) ++ " width: "
                            ++ (last numList) ++ " "
          where
            numList = words nums
            
--outputPart is a helper function for makeOutput which turns a list
--of Int into a readable String for output

outputPart outList = "Sides of Squares: " ++
                    (unwords $ map show outList)

--processInput is a recursive function.  Each call reads one line of input,
--calls solve to do the processing, calls makeOutput to make the results
--readable, and prints the results of processing one line to a file

processInput inFileHandle outFileHandle = do
  eof <- hIsEOF inFileHandle
  if eof 
    then return ()
   else do
      inputLine <- hGetLine inFileHandle
      let list = solve inputLine
      let outputLine = makeOutput inputLine list
      hPutStrLn outFileHandle outputLine
      processInput inFileHandle outFileHandle
{-
main is the entry point to the program.  Compile the code with command
     ghc --make Main.hs
then run the program with command
     ./Main sqInput sqOut
where sqInput is the name of the input file and sqOut is the name of the
output file.
-}

main = do
  [inFile, outFile] <- getArgs
  inFileHandle <- openFile inFile ReadMode
  outFileHandle <- openFile outFile WriteMode
  hPutStrLn outFileHandle "               Rectangle to Squares"
  hPutStrLn outFileHandle ""
  hPutStrLn outFileHandle "Rectangle Dimensions        Constructed Squares"
  hPutStrLn outFileHandle ""
  processInput inFileHandle outFileHandle
  hClose inFileHandle
  hClose outFileHandle
  
