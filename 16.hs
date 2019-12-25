
digits :: Integral x => x -> [x]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

main = print $ sum $ digits (2^1000)