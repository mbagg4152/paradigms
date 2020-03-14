module Main where

import System.Environment
import System.IO
import Data.Char
import Data.Function
import Data.List

main = do
    fname <- getArgs
    fcontent <- readFile (fname!!0)
    let flines = lines fcontent
    putStrLn fcontent
    let kSize = read ( flines!!0) :: Int
    let kss = boardSize kSize
    putStrLn ("board size: " ++ kss)



boardSize :: Int -> String
boardSize sze
    | sze == 4 = "four"
    | sze == 5 = "five"
    | sze == 6 = "six"
    | otherwise = "not a size"


