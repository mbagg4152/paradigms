module Main where
import System.Environment
import System.IO
import Data.Char
import Data.List



main = do
    s<- readFile "hello.txt"
    let t = remove s
    let q = sort $ words t
    doAThing $unwords q

doAThing :: String -> IO ()
doAThing str = putStrLn str

remove:: [Char] -> [Char]
remove (x:xs)
    | length xs == 0 =[x]
    | isAlpha x      = x:remove xs
    | otherwise      =' ':remove xs