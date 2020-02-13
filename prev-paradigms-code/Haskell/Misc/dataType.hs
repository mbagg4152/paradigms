
--example pattern matching
data BookInfo = Book String [String]
  deriving (Show)

cBook :: BookInfo
cBook = Book "C, A Reference Manual" ["Harbison", "Steele"]


title (Book x xs) = x

authors (Book x xs) = xs

--example naming fields

data Library = Text {
                      name :: String,
                      writers :: [String]}
                      deriving (Show)

lBook :: Library
lBook = Text {
               name = "C, A Reference Manual",
               writers = ["Harbison", "Steele"] }

libAuth x = writers x

