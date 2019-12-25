digits :: Integral x => x -> [x]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

is_pal n = digits n == reverse (digits n)

main = print $ maximum [x*y | x <- [1..1000] , y <- [1..x], is_pal (x*y)]