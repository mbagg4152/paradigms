{-	Bradley Gardner and Nathan Smith
	CS 231 Section 03L
	Haskell Lab Assignment 5 - Evil Hangman
	hang.hs
	
	The purpose of this program is to frustrate whosoever is unfortunate enough 
	to be convinced to play this game. It will take every input they give it and pick
	the group of words that is largest either including the letter or not. The user 
	has to somehow get this list of words down to one before they can win. It will 
	attempt to make it as difficult as possible to win for the player. 
	
	The program algorithm is:
	Check to make sure file exists
	Read the file
	Check to ensure parameters are valid
	Process the dictionary file
	Calls outerLoop which sets up the game and calls innerLoop
	InnerLoop then controls the actual game play and when finished returns to outerLoop
	OuterLoop then either exits if player/victim does not want to play or calls playAgain if they do
-}

-- Imports
module Main where
import System.IO
import System.Environment
import System.Exit
import System.Directory
import Data.Char
import Data.List

-- Checks to make sure the file exists and returns the file if it does
fileCheck file = do
 exist <- doesFileExist file
 -- File Exists, return file
 if exist
  then return file
 -- File does not exist
 else
  exitFailure

-- Checks to make sure there are the proper number of arguments
argCheck = do
 -- Gets the arguments from command line
 args <- getArgs
 let len = length args
 -- Checks for proper number of arguments
 if len == 3 || len == 4
 then 
  -- adds a '-' to the args for later use
  if len == 3
   then do
    let augArgs = args++["-"]
    return augArgs
  -- Returns unedited args
  else return args
 -- Improper number of args
 else do 
  putStrLn "Invalid number of arguments"
  exitFailure

-- Replaces the dash with the correct letter according to the pattern given to it
dashReplace pattern dash
 | null pattern         = [] -- End list
 | (head pattern) ==' ' = [head dash] ++ dashReplace (tail pattern) (tail dash)        -- Recursive call and leaves '-'
 | otherwise            = [[head pattern]] ++ dashReplace (tail pattern) (tail dash)   -- Recursive call and replaces '-'

-- Checks to see if each item in a list is true
truthCheck truths
 | null truths = True                     -- Empty list
 | head truths = truthCheck $ tail truths -- Recursive call to check rest of list
 | otherwise   = False                    -- Returns false

-- Declares the winner: Player 1 (computer/programmer) or Player 2 (current victim)
declareWin win word
 | win       = putStrLn $ "You win!!!! You Guessed: " ++ word ++ " correctly!"                 -- Player 2 win
 | otherwise = putStrLn $ "MISSION FAILED... WE'LL GET 'EM NEXT TIME \nThe word was " ++ word  -- Player 1 win

-- Used in pattern, changes a character to a space if not equal to selected character
match c1 c2
 |c1 == c2  = c1   -- Characters match, return matching char
 |otherwise = ' '  -- Characters do not match, return ' '

-- Controls the max number of guesses possible
maxGuess guess
 | guess > 15 = 15     -- User tried to get more than 15 guesses
 | otherwise  = guess  -- User put a value less than 15 guesses

-- Creates a string consisting of '_' and ' '
dashedWord size 
 | size == 1 = "_"                           -- Return a single dash if there is only one 
 | otherwise = "_ " ++ (dashedWord $ size-1)  -- Return a dash and a space and con whatever is returned from the recursive call

-- Tells user the letters they have already guessed, in alphabetical order
printGuessed letters
 | length letters == 0 = "No letters have been guessed yet\n"                      -- Returns that no letters guessed yet
 | otherwise           = "Guessed letters: " ++ (unwords $ sort letters) ++"\n"    -- Returns list of letters guessed

-- Prints number of words in family if debug mode is valid
debugMode db len
 | db == "-n" = "Number of items in family: " ++ (show len) -- Debug is valid, return num of words in family
 | otherwise  = ""                                          -- Debug is invalid, return empty string

-- Creates a new family based on the pattern and letter given
patternMatch strings letterPattern letter
 | null strings = [] -- Empty/End of list
 | (pattern letter (head strings)) == letterPattern = [head strings] ++ patternMatch (tail strings) letterPattern letter -- Recursive call while adding item to list
 | otherwise    = [] ++ patternMatch (tail strings) letterPattern letter -- Recursive call without adding current item to list

-- Prints the information to console to inform the user by calling above methods and printing their return strings
updateVictim letters guesses db dashed len = "Number of remaining guesses: "
 ++ (show guesses) ++ "\n" ++ printGuessed letters 
 ++ "The current word is " ++ dashed ++"\n" ++ debugMode db len

-- Used in determining the new family of words
wordsOfLength list size = filter ((\a b -> (length b) == a) size) list

-- Takes in a string and a character, if a character in the string does not equal the the selected character, replace with a space
pattern char string = map (match char) string  -- Calls map to match each char in string

-- Updates the family to account for user input 
adjustFamily strings letter = do
 let patterns = map (pattern letter) strings                                    -- Coverts all members of the strings list to patterns
 let patternsList = group $ sort patterns                                       -- Sorts the pattern and groups them into families
 let maxList = maximumBy (\a b -> compare (length a) (length b)) patternsList   -- Selects the largest pattern family
 let augList = patternMatch strings (head maxList) letter                       -- Gets the largest family of possible words
 return augList                                                                 -- Returns the largest family

-- Checks for proper input from user (used for input error handling)
inputHelper = do
 entry <-getLine                      -- Gets user input
 if (length entry) > 0                -- Checks to make sure its not an empty line
  then return entry                   -- Returns non empty input
 else do
  putStrLn "Please enter a valid input\n" -- Tells user to enter data (if they entered an empty line)
  inputHelper                         -- Recursive call

-- Checks for valid numbers from user
numValid size phrase= do
 let boolList =map isDigit size       -- Checks to make sure each value is a digit and puts result into boolean array
 if (truthCheck boolList)             -- Goes through an makes sure all values are true (all are digits)
  then do
   let num = read size :: Int         -- Converts from String to Int
   return num                         -- Returns number
 else do
  putStrLn $ "Invalid " ++ phrase ++ " provided" -- Tells user input was invalid
  exitFailure
  
-- Outer Loop controls game setup
outerLoop dictContents size guess db = do
 let sizeWords = wordsOfLength dictContents size             -- Gets words from dictionary that has length of 'size'
 let dashed = dashedWord size                                -- Sets up the dashed string to represent number of char in word
 let guessedLetters = []                                     -- Sets up list of guessed Letters
 if (length sizeWords) > 0                                   -- Makes sure there is at least one word of that length
  then do
   innerLoop dashed sizeWords guess db guessedLetters        -- Calls innerLoop which controls game play
   putStrLn "Would you like to play again (Y/N)?"            -- Ask user if they want to play again
   again <- inputHelper                                      -- Gets user input on whether or not they wish to play again
   let capAgain = map toUpper again                          -- Calls toUpper to make comparison easier
   if (head capAgain) == 'Y'                                 -- Check if user wants to play again
    then playAgain dictContents size guess db                -- Call playAgain to set up new parameters
   else
    putStrLn "GAME OVER... FATALITY"                         -- User quit the game
 else
  putStrLn $ "There are no words of length " ++ (show size)  -- There were no words of the user requested length

-- Inner Loop controls the actual gameplay
innerLoop dashed sizeWords guess db guessedLetters = do
 let win = not $ elem '_' dashed    -- Sets the win conition (no '_' left)
 let outOfGuess = guess > 0         -- Makes sure user has at least one guess left
 let len = length sizeWords         -- Length of the words available
 if not win && outOfGuess           -- Checks to make sure user didn't win and still has guesses left
  then do
   putStrLn $ updateVictim guessedLetters guess db dashed len              -- Prints the game info to update player
   putStrLn "Please enter your guess (Single letter)"                      -- Tells user to make a guess
   userGuess <- getLine                                                    -- Gets the players guess
   let upper = map toUpper userGuess                                       -- Makes everything upper case for comparison
   let validLength = (\a -> (length a) > 0) userGuess                      -- Makes sure user actually entered data
   if validLength                                                          -- If the length is valid:
    then do
     let validChar = isAlpha $ head userGuess                              -- Checks to make sure input is valid letter
     let unused = not $ elem [head upper] guessedLetters                   -- Makes sure letter was not already guessed
     if validChar && unused
      then do
       newFamily <- adjustFamily sizeWords (head upper)                    -- Adjusts the current family to account for user letter
       let addedUsed = guessedLetters ++ [[(head upper)]]                  -- Adds the guessed letter to the list of guessed letters
       let wordPattern = pattern (head upper) (head newFamily)             -- Update the word pattern 
       let updatedDash = unwords $ dashReplace wordPattern (words dashed)  -- Update the string of dashes and letters with current pattern and letter
       if updatedDash == dashed                                            -- Checks whether or not to subtract a guess, if the player got a letter
        then innerLoop updatedDash newFamily (guess -1) db addedUsed       -- Calls innerLoop with one less guess and updated information
       else
        innerLoop updatedDash newFamily guess db addedUsed                 -- Calls innerLoop with updated information (same num of guesses)
     else do
      putStrLn "Please enter a valid, unused input\n"                                -- Tells user to enter valid input
      innerLoop dashed sizeWords guess db guessedLetters                   -- Calls innerLoop with updated information (same num of guesses)
   else do
    putStrLn "Please enter a valid input\n"                                  -- Tells user to enter valid input
    innerLoop dashed sizeWords guess db guessedLetters                     -- Calls innerLoop with updated information (same num of guesses)
 else
  declareWin win (head sizeWords)                                          -- Declares the winner

-- PlayAgain asks the user if they want to update the parameters of the game
playAgain dictionary size guess db = do
 putStrLn "Would you like to change the number of guesses? (Y/N)" -- Asks user if they want to change number of guesses
 guessAmounts <- pickNew guess                              -- Gets user answer
 let augGuess = maxGuess guessAmounts                       -- Makes sure the number of guesses is 15 or less
 putStrLn "Would you like to change the word length? (Y/N)" -- Asks user if they want to change the word length
 letterAmount <- pickNew size                               -- Gets the users answer
 outerLoop dictionary letterAmount augGuess db              -- Calls outerLoop with the update parameters
 
-- Gets new parameters from user and returns them 
pickNew old = do
 ans <- inputHelper                              -- Gets result from inputHelper
 let up = map toUpper ans                        -- Converts to upper case for comparison
 if (head up) == 'Y'                             -- Checks if the user said yes
  then do
   putStrLn "Enter your new value (between 1 and 15)" -- Requests input
   newEnter <- inputHelper                       -- Gets result from inputHelper
   let valid = truthCheck $ map isDigit newEnter -- Checks if valid digits
   if valid                                      -- Checks to make sure valid is true
    then do
     let num = read newEnter :: Int              -- Reads the input as an Int instead of a String
     return num                                  -- Returns the number given
   else do
    putStrLn"ERROR INVALID ENTRY!!"
    exitFailure
 else
  return old                                     -- Returns original parameters

-- Main controls the program flow 
main = do
 [dictionary, wLength, guesses, db] <- argCheck       -- Gets augmented arguments
 validDict <- fileCheck dictionary                    -- Checks to see if file exists
 dict <- readFile validDict                           -- Reads the file that exists
 validSize <- numValid wLength "word length"          -- Makes sure the word length is valid
 validGuess <- numValid guesses "number of guesses"   -- Makes sure the number of guesses is valid
 let correctedGuesses = maxGuess validGuess           -- Corrects number of guesses if over 15
 let dictContents = lines $ map toUpper dict          -- Splits dictionary into uppercase list
 outerLoop dictContents validSize correctedGuesses db -- Calls outerLoop to set up the game play