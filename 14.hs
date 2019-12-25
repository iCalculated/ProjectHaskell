    

-- main = print $ maximum $ map collatz starts where
main = print $ (+1).length $ takeWhile (/=525) $ map collatz starts where
    collatz n 
        |   n == 1            = 1
        |   n `mod` 2 == 0    = 1 + collatz (n `div` 2)
        |   n `mod` 2 == 1    = 1 + collatz (3 * n + 1)
    starts = [1..1000000]

