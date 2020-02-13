import Text.Read
import System.Directory
import Data.Maybe


-- Maybe data type
divide :: Int -> Int -> Maybe Int
divide _ 0 = Nothing
divide x y = Just (div x y)


-- test for existence of file

main = do
  t <- doesFileExist "maybe.hs"
  putStrLn $ show t
  let x = divide 30 0
  putStrLn $ show x
  let y = divide 30 4
  let z = fromJust y
  putStrLn $ show z

