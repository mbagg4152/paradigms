-- General Recursion Example
-- implement fibonacci number directly from its definition

type Thing = (String, Int)

other:: Thing -> Int
other p = snd p

square2 n
 | n `mod` 2 == 0 = myEven n
 | otherwise      = myOdd n

myEven n = (2^(n `div` 2))^2
myOdd n = ((2^((n-1)`div`2))^2)*2
--This was a bad idea in C
--for the same reason, it is a bad idea in Haskell