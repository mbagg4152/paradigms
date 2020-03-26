import System.Environment 
import Data.Char 
import Data.Maybe 
import Control.Monad.State

-- a puzzle has a size (so we know the limit of values to use)
-- its original input as a list of strings (just in case we want to print it)
-- a list of constraints 
-- and a list of cells with position/label
-- the cells could be a list of lists, but doing the lookup in a data set
-- this size isn't likely to be the limiting factor and we'll abstract
-- over getting a cell by x,y coordinates anyway 

data Puzzle = Puzzle { 
      psize :: Int, 
      origInput ::  [String],  
      constraints :: [Constraint], 
      pcells :: [Cell] 
    } 
   
-- constraints have labels (from the input description)
-- operations (the arithmetic operators as strings)
-- target values 
-- and a list of the cells that make up the constraint 
data Constraint =  Constraint 
                   { conlabel :: String,
                     conop :: String,
                     contarget :: Int,
                     concells :: [Cell] 
                   }
                  deriving Show 

-- each cell in the puzzle has a position (cx, cy) and a label
-- corresponding to the constraint it is in 
data Cell = Cell {
              clabel ::  String,
              cx :: Int,
              cy :: Int
          }
          deriving (Eq, Show)

-- an assignment is, well, an assignment of a value to a cell

data Assignment = Assignment { acell :: Cell, avalue :: Int } 
                deriving (Show, Eq)

-- a possibility represents a "possible" solution to the puzzle

type Possibility = [ Assignment ] 

-- The PuzzleM type contains the base puzzle

type PuzzleM = StateT Puzzle IO

getPuzzle :: PuzzleM Puzzle 
getPuzzle =  get 

getConstraints = do 
  p <- getPuzzle 
  return $ constraints p 

-- not a fancy show, but shows the pieces - quick and easy 
instance Show Puzzle where
   show (Puzzle{psize=s, origInput=inp,constraints=cos,pcells=ces}) = 
     unlines $ ["Puzzle::", "size="++(show s)] 
                 ++ inp
                 ++  (map show cos) 
                 ++ (map show ces)







strip l = sl 
  where 
      sl = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace l 

parse :: String -> Puzzle
parse s = Puzzle {psize=size, origInput=plines, constraints=constraintList, pcells=cellList}
          where 
            plines = map strip $ lines s 
            (cellLines, constraintLines) = break ([]==) plines
            size = length cellLines 
            cellList = doCellLines 0 cellLines 
            constraintList = parseConstraintLines cellList $ tail constraintLines 
          --putStrLn pLines
doCellLines :: Int -> [String] -> [Cell] 
doCellLines i []     = [] 
doCellLines i (l:ls) = let l1 = zip [0..] l 
                           mkcell (xpos, y) = Cell { clabel=[y], cx=xpos, cy=i} 
                           l2 = map mkcell l1 
                           in l2 ++ (doCellLines (i+1) ls) 

parseConstraintLines cells lines = map (parseConstraint cells) (filter ("" /=) lines)

parseConstraint cells l =  Constraint {conlabel= label, 
                                   conop = op, 
                               contarget = target,
                                   concells = clist } 
                    where 
                      (label,rest) = break ('='==) l 
                      (starget, op) = break (not.isDigit) $ tail rest 
                      target = read starget         
                      clist = filter (\c -> clabel c == label) cells 





showPuzzle = do 
                p <- getPuzzle
                liftIO $ putStrLn $ show p 

solve :: [Cell] -> Possibility -> PuzzleM Possibility
solve [] assList  = return assList 
solve cl@(c:cs) assList  = do 
                           s <- psize `liftM` getPuzzle 
                           let pass = map (\v -> Assignment{ acell=c, avalue=v}) [1..s]
                               passes = map (:assList) pass 
                           solve1 cs passes 

solve1 cells [] = return [] 
solve1 cells pl@(p:ps) = do 
                        good <- okSoFar p 
                        if good
                           then do solved <- solve cells p
                                   if solved /= [] 
                                      then return solved
                                      else solve1 cells ps 
                           else solve1 cells ps 

allRowsOK p = do 
                s <- psize `liftM` getPuzzle
                return $ and $ map (rowOK s p) [0..s-1] 

allColsOK p = do 
                s <- psize `liftM` getPuzzle
                return $ and $ map (colOK s p) [0..s-1] 

rowOK s plist row = allDifferent (map avalue inrow) 
    where 
      inrow = filter (\x -> (row == (cy $ acell x))) plist 

colOK s plist col = allDifferent (map avalue incol) 
    where 
      incol = filter (\x -> (col == (cx $ acell x))) plist 
                               
allDifferent [] = True 
allDifferent (x:xs) = (not $ elem x xs) && allDifferent xs 

allConsOK p = do 
                 conlist <- constraints `liftM` getPuzzle  
                 return $ and $ map (conOK p) conlist 

conOK p constraint = checkCon convals contype target cl
    where 
      concl = concells constraint 
      convals = map avalue $ filter (\x -> ( acell x) `elem` concl) p
      contype = conop constraint 
      target = contarget constraint 
      cl = length concl 

checkCon [] _   tgt _ = True       
checkCon cl "=" tgt _ = tgt == cl !! 0
checkCon cl "*" tgt l = if length cl == l 
                           then tgt == product cl 
                           else 0== tgt `mod` (product cl) 
checkCon cl "+" tgt l = if length cl == l
                           then tgt == sum cl 
                           else tgt >= sum cl 
checkCon cl "-" tgt _
  | length cl > 2   = False 
  | length cl == 2  = abs(cl !! 0 - cl !! 1) == tgt 
  | length cl == 1  = True

checkCon cl "/" tgt  _
  | length cl > 2   = False 
  | length cl == 1  = True 
  | length cl == 2  = let a = cl !! 0 
                          b = cl !! 1
                      in  (a `div` b) == tgt  || (b `div` a) == tgt

okSoFar p = do 
               rowsOK <- allRowsOK p 
               colsOK <- allColsOK p 
               consOK <- allConsOK p 
               return $ rowsOK && colsOK && consOK 

showKnownCells al s =   unlines $ map getRow [0..s-1] 
                    where 
                      getCellByRowCol al r c = filter (\x -> (r == (cy $ acell x)) && (c == (cx $ acell x))) al 
                      getRow r = unwords $ map doCell $ map (getCellByRowCol al r) [0..s-1] 
                      doCell [] = " "
                      doCell (x:xs) = show $ avalue x 
                      

runPuzzle  = do
                {- showPuzzle  -} 
                cl <- pcells `liftM` getPuzzle 
                solve cl [] 

showPossibles p =   unlines $ map show p

main = do 
          args <- getArgs 
          doPuzzle (args !! 0)


doPuzzle fn = do 
  inp <- readFile fn 
  let puzzle = parse inp 
  
  putStrLn inp 
  putStrLn "about to evalState puzzle..."
  (soln,p) <- runStateT runPuzzle puzzle
  putStrLn $ showKnownCells soln 6
