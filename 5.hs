-- I know it's bad. I just don't know enough Haskell yet.
main = print $ take 1 (filter (\x -> all (\l -> x `mod` l == 0) [1..20]) [20 * x | x <- [1..]])
